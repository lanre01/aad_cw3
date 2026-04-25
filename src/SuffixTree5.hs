{-# LANGUAGE ScopedTypeVariables #-}

-- | Ukkonen-style suffix tree over strict 'ByteString' values.
--
-- Construction uses a mutable hash table ('Data.HashTable.ST.Basic') for
-- O(1) amortised node operations within the 'ST' monad.  Edge maps remain
-- 'IntMap'-keyed by 'Word8' values (bounded at 256 entries).
--
-- After construction the node table is frozen into a boxed 'Vector', exploiting
-- the fact that node identifiers are allocated sequentially from 0.  This gives
-- O(1) node lookup during queries and an O(n) freeze step.
module SuffixTree5 (
    STree (..),
    Edge (..),
    Node (..),
    sTree,
    containsString
) where

import qualified Data.ByteString as B
import qualified Data.IntMap.Strict as IMap
import qualified Data.HashTable.ST.Basic as H
import qualified Data.HashTable.Class as HC
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Control.Monad (when)
import           Control.Monad.ST
import           Data.STRef
import           Data.Word (Word8)

type InputText = B.ByteString

type NodeId = Int

-- | Strict pair of 'Int' values used as an allocation-conscious return type.
data IntPair = IP {-# UNPACK #-} !Int
                  {-# UNPACK #-} !Int

-- | Strict triple of 'Int' values used while walking edge intervals.
data IntTriple = IT {-# UNPACK #-} !Int
                    {-# UNPACK #-} !Int
                    {-# UNPACK #-} !Int

-- | Specialised strict optional edge end.
data StrictIntMaybe
  = SNothing
  | SJust {-# UNPACK #-} !Int deriving Show

-- | Directed edge labelled by a slice of the indexed input.
data Edge = Edge
  { start  :: {-# UNPACK #-} !Int
  , end    :: !StrictIntMaybe
  , target :: {-# UNPACK #-} !Int
  } deriving Show

-- | Tree node with its suffix link and outgoing byte transitions.
data Node = Node
  { suffixLink :: !StrictIntMaybe
  , children   :: !(IMap.IntMap Edge)
  } deriving Show

-- | Complete suffix tree plus the immutable text it indexes.
--
-- The node table is a boxed 'Vector' indexed directly by 'NodeId'.
-- Since node ids are allocated sequentially from 0, every id is a valid
-- index and lookup is O(1).
data STree = STree
  { text     :: !InputText
  , nodes    :: !(V.Vector Node)
  , rootId   :: {-# UNPACK #-} !NodeId
  , bottomId :: {-# UNPACK #-} !NodeId
  } deriving Show

-- | Mutable construction environment parameterised by the 'ST' state token.
data BuildEnv s = BuildEnv
  { envNodes    :: !(H.HashTable s Int Node)
  , envText     :: !InputText
  , envRootId   :: {-# UNPACK #-} !Int
  , envBottomId :: {-# UNPACK #-} !Int
  , envNextId   :: !(STRef s Int)
  , envActiveS  :: !(STRef s Int)
  , envActiveK  :: !(STRef s Int)
  }

emptyNode :: Node
emptyNode = Node { suffixLink = SNothing, children = IMap.empty }

-- | Create an empty node with a known suffix link.
mkNode :: NodeId -> Node
mkNode sid = Node { suffixLink = SJust sid, children = IMap.empty }

-- | Resolve a stored edge end to a concrete inclusive index.
resolveEdgeEnd :: Int -> StrictIntMaybe -> Int
resolveEdgeEnd currentMax SNothing  = currentMax
resolveEdgeEnd _          (SJust a) = a

-- | Initialise the mutable build environment.
initBuildEnv :: InputText -> ST s (BuildEnv s)
initBuildEnv input = do
    let bId = 0
        rId = 1
    ht   <- H.newSized (B.length input * 2)
    H.insert ht bId  emptyNode
    H.insert ht rId (mkNode bId)
    nRef <- newSTRef 2
    sRef <- newSTRef rId
    kRef <- newSTRef 0
    pure BuildEnv
      { envNodes    = ht
      , envText     = input
      , envRootId   = rId
      , envBottomId = bId
      , envNextId   = nRef
      , envActiveS  = sRef
      , envActiveK  = kRef
      }

-- | Allocate a fresh node id and insert an empty node into the hash table.
newNode :: BuildEnv s -> ST s NodeId
newNode env = do
    nid <- readSTRef (envNextId env)
    writeSTRef (envNextId env) $! nid + 1
    H.insert (envNodes env) nid emptyNode
    pure nid

-- | Look up a node by id, failing if the table is inconsistent.
lookupNode :: BuildEnv s -> NodeId -> ST s Node
lookupNode env nid = do
    r <- H.lookup (envNodes env) nid
    case r of
        Just n  -> pure n
        Nothing -> error "Node not present"

-- | Set an outgoing edge for a byte transition on a node.
setTransition :: BuildEnv s -> NodeId -> Word8 -> Edge -> ST s ()
setTransition env n t edge = do
    nd <- lookupNode env n
    let nd' = nd { children = IMap.insert (fromIntegral t) edge (children nd) }
    H.insert (envNodes env) n nd'

-- | Set or replace the suffix link for a node created during a split.
setSuffixLink :: BuildEnv s -> NodeId -> NodeId -> ST s ()
setSuffixLink env n tid = do
    nd <- lookupNode env n
    H.insert (envNodes env) n (nd { suffixLink = SJust tid })

-- | Check whether a node has an outgoing transition for the given byte.
hasTransition :: BuildEnv s -> NodeId -> Word8 -> ST s Bool
hasTransition env s t
    | s == envBottomId env = pure True
    | otherwise = do
        nd <- lookupNode env s
        pure $! IMap.member (fromIntegral t) (children nd)

-- | Look up the transition selected by the byte at text index @k@.
lookupTransition :: BuildEnv s -> NodeId -> Int -> ST s Edge
lookupTransition env s k
    | s == envBottomId env =
        pure Edge { start = k, end = SJust k, target = envRootId env }
    | otherwise = do
        snode <- lookupNode env s
        let key = fromIntegral $ B.index (envText env) k
        case IMap.lookup key (children snode) of
            Just edge -> pure edge
            Nothing   -> error "Edge not present"

-- | Return the transition target and concrete label bounds for an active point.
findTkTransition :: BuildEnv s -> NodeId -> Int -> Int -> ST s IntTriple
findTkTransition env s k maxIdx = do
    edge <- lookupTransition env s k
    let edgeEnd = resolveEdgeEnd maxIdx (end edge)
    pure (IT (target edge) (start edge) edgeEnd)

-- | Follow a node's suffix link.
nodeSuffixLink :: BuildEnv s -> NodeId -> ST s NodeId
nodeSuffixLink env nid = do
    nd <- lookupNode env nid
    case suffixLink nd of
        SJust link -> pure link
        SNothing   -> error "Missing suffix link"

-- | Canonicalise an active point to the closest explicit node or edge start.
canonize :: forall s. BuildEnv s -> NodeId -> (Int, Int) -> Int -> ST s IntPair
canonize env s (k, i) maxIdx
    | i < k     = pure $! IP s k
    | otherwise = do
        res <- findTkTransition env s k maxIdx
        go (IT s k i) res
  where
    go :: IntTriple -> IntTriple -> ST s IntPair
    go (IT s0 k0 p0) (IT s1 k1 p1)
        | p1 - k1 <= p0 - k0 = do
            let k2 = k0 + p1 - k1 + 1
            if k2 <= p0
                then do
                    tkTrans <- findTkTransition env s1 k2 maxIdx
                    go (IT s1 k2 p0) tkTrans
                else pure $! IP s1 k2
        | otherwise = pure $! IP s0 k0

-- | Test whether the active path already continues with a byte or needs a split.
testAndSplit :: BuildEnv s -> NodeId -> (Int, Int) -> Word8 -> ST s (Bool, NodeId)
testAndSplit env s (k, p) t
    | k <= p = do
        edge <- lookupTransition env s k
        let k'       = start edge
            splitEnd = k' + p - k
            nextChar = B.index (envText env) (splitEnd + 1)
        if t == nextChar
            then pure (True, s)
            else do
                r <- newNode env
                let prefixEdge = Edge { start = k', end = SJust splitEnd, target = r }
                    suffixEdge = Edge { start = splitEnd + 1, end = end edge, target = target edge }
                setTransition env s (B.index (envText env) k') prefixEdge
                setTransition env r nextChar suffixEdge
                pure (False, r)
    | otherwise = do
        exists <- hasTransition env s t
        pure (exists, s)

-- | Add a leaf edge for the suffix beginning at the given index.
addLeaf :: BuildEnv s -> NodeId -> Int -> ST s ()
addLeaf env parent startIdx = do
    leafId <- newNode env
    let edge = Edge { start = startIdx, end = SNothing, target = leafId }
        firstChar = B.index (envText env) startIdx
    setTransition env parent firstChar edge

-- | Run Ukkonen's update step for one input position.
update :: forall s. BuildEnv s -> Int -> ST s (NodeId, Int)
update env maxIdx = do
    s <- readSTRef (envActiveS env)
    k <- readSTRef (envActiveK env)
    let currentChar = B.index (envText env) maxIdx
    (endPoint, r) <- testAndSplit env s (k, maxIdx - 1) currentChar
    go currentChar (envRootId env) s k endPoint r
  where
    go :: Word8 -> NodeId -> NodeId -> Int -> Bool -> NodeId -> ST s (NodeId, Int)
    go currentChar oldr currentS currentK endPoint r
        | endPoint = do
            when (oldr /= envRootId env) $
                setSuffixLink env oldr currentS
            pure (currentS, currentK)
        | otherwise = do
            addLeaf env r maxIdx
            when (oldr /= envRootId env) $
                setSuffixLink env oldr r
            nextS <- nodeSuffixLink env currentS
            (IP canonS canonK) <- canonize env nextS (currentK, maxIdx - 1) maxIdx
            (endPoint', r') <- testAndSplit env canonS (canonK, maxIdx - 1) currentChar
            go currentChar r canonS canonK endPoint' r'

-- | Extend the tree so it represents suffixes ending at the given input index.
extendAtIndex :: BuildEnv s -> Int -> ST s ()
extendAtIndex env i = do
    (s', k') <- update env i
    (IP s'' k'') <- canonize env s' (k', i) i
    writeSTRef (envActiveS env) s''
    writeSTRef (envActiveK env) k''

-- | Build the tree in 'ST' and freeze the hash table into a 'Vector'.
--
-- Node ids are sequential integers from 0, so the hash table entries are
-- written into a mutable vector at their id positions and then frozen.
buildAndFreeze :: forall s. InputText -> ST s STree
buildAndFreeze input = do
    env <- initBuildEnv input
    let lastIdx = B.length input - 1
        loop i
          | i > lastIdx = pure ()
          | otherwise   = extendAtIndex env i >> loop (i + 1)
    loop 0
    -- Freeze: hash table -> MVector -> Vector
    nodeCount <- readSTRef (envNextId env)
    mv <- MV.new nodeCount
    HC.mapM_ (\(k, v) -> MV.write mv k v) (envNodes env)
    frozen <- V.unsafeFreeze mv
    pure STree
      { text     = input
      , nodes    = frozen
      , rootId   = envRootId env
      , bottomId = envBottomId env
      }

-- | Build a suffix tree containing every suffix of the input.
--
-- Construction uses a mutable hash table internally via the 'ST' monad;
-- the result is frozen to an immutable 'Vector' so that queries remain pure
-- with O(1) node access.
sTree :: InputText -> STree
sTree input = runST (buildAndFreeze input)

-- | Check whether a pattern occurs in the indexed text.
containsString :: STree -> InputText -> Bool
containsString _ patternText
    | B.null patternText = True
containsString tree patternText = goNode (rootId tree) 0
  where
    patternLen = B.length patternText
    textLen    = B.length (text tree)
    nodeVec    = nodes tree

    goNode currentNode patternIdx
        | patternIdx >= patternLen = True
        | otherwise =
            let current = nodeVec V.! currentNode
                key     = fromIntegral (B.index patternText patternIdx)
            in case IMap.lookup key (children current) of
                Nothing   -> False
                Just edge -> goEdge edge patternIdx (start edge)

    goEdge edge patternIdx textIdx
        | patternIdx >= patternLen = True
        | textIdx > edgeStop edge  = goNode (target edge) patternIdx
        | textIdx >= textLen       = False
        | B.index patternText patternIdx == B.index (text tree) textIdx =
            goEdge edge (patternIdx + 1) (textIdx + 1)
        | otherwise = False

    edgeStop edge = resolveEdgeEnd (textLen - 1) (end edge)