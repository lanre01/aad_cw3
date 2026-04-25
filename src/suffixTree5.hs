-- | Ukkonen-style suffix tree over strict 'ByteString' values.
--
-- This variant replaces the persistent 'IntMap' node table used during
-- construction with a mutable 'BasicHashTable' from the @hashtables@ package.
-- Edge maps remain 'IntMap'-keyed by 'Word8' values (bounded at 256 entries),
-- so their cost is unchanged.  The node table is converted to an immutable
-- 'IntMap' after construction so that 'containsString' stays pure.
--
-- The external API is identical to 'SuffixTree4': 'sTree' is pure (via
-- 'unsafePerformIO') and 'containsString' operates on the frozen tree.
module SuffixTree4HT (
    STree (..),
    Edge (..),
    Node (..),
    sTree,
    containsString
) where

import qualified Data.ByteString as B
import qualified Data.IntMap.Strict as IMap
import qualified Data.HashTable.IO as H
import           Control.Monad (when)
import           Data.IORef
import           Data.Word (Word8)
import           System.IO.Unsafe (unsafePerformIO)

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
  { suffixLink :: !(Maybe NodeId)
  , children   :: !(IMap.IntMap Edge)
  } deriving Show

-- | Complete suffix tree plus the immutable text it indexes.
--
-- This is the frozen representation returned by 'sTree'.  The node table is
-- an ordinary 'IntMap' so that queries can run without IO.
data STree = STree
  { text     :: !InputText
  , nodes    :: !(IMap.IntMap Node)
  , rootId   :: {-# UNPACK #-} !NodeId
  , bottomId :: {-# UNPACK #-} !NodeId
  } deriving Show

-- | Mutable construction environment.
--
-- All node storage goes through 'envNodes', a 'BasicHashTable' with O(1)
-- amortised lookup and insert.  Scalar state (next id, active point) lives
-- in 'IORef's for in-place update.
data BuildEnv = BuildEnv
  { envNodes    :: !(H.BasicHashTable Int Node)
  , envText     :: !InputText
  , envRootId   :: {-# UNPACK #-} !Int
  , envBottomId :: {-# UNPACK #-} !Int
  , envNextId   :: !(IORef Int)
  , envActiveS  :: !(IORef Int)
  , envActiveK  :: !(IORef Int)
  }

emptyNode :: Node
emptyNode = Node { suffixLink = Nothing, children = IMap.empty }

-- | Create an empty node with a known suffix link.
mkNode :: NodeId -> Node
mkNode sid = Node { suffixLink = Just sid, children = IMap.empty }

-- | Resolve a stored edge end to a concrete inclusive index.
resolveEdgeEnd :: Int -> StrictIntMaybe -> Int
resolveEdgeEnd currentMax SNothing  = currentMax
resolveEdgeEnd _          (SJust a) = a

-- | Initialise the mutable build environment.
--
-- The hash table is pre-sized to @2 * n@ entries since a suffix tree contains
-- at most @2n - 1@ nodes.
initBuildEnv :: InputText -> IO BuildEnv
initBuildEnv input = do
    let bId  = 0
        rId  = 1
    ht <- H.newSized (B.length input * 2)
    H.insert ht bId  emptyNode
    H.insert ht rId (mkNode bId)
    nRef <- newIORef 2
    sRef <- newIORef rId
    kRef <- newIORef 0
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
newNode :: BuildEnv -> IO NodeId
newNode env = do
    nid <- readIORef (envNextId env)
    writeIORef (envNextId env) $! nid + 1
    H.insert (envNodes env) nid emptyNode
    pure nid

-- | Look up a node by id, failing if the table is inconsistent.
lookupNode :: BuildEnv -> NodeId -> IO Node
lookupNode env nid = do
    r <- H.lookup (envNodes env) nid
    case r of
        Just n  -> pure n
        Nothing -> error "Node not present"

-- | Set an outgoing edge for a byte transition on a node.
setTransition :: BuildEnv -> NodeId -> Word8 -> Edge -> IO ()
setTransition env n t edge = do
    nd <- lookupNode env n
    let nd' = nd { children = IMap.insert (fromIntegral t) edge (children nd) }
    H.insert (envNodes env) n nd'

-- | Set or replace the suffix link for a node created during a split.
setSuffixLink :: BuildEnv -> NodeId -> NodeId -> IO ()
setSuffixLink env n tid = do
    nd <- lookupNode env n
    H.insert (envNodes env) n (nd { suffixLink = Just tid })

-- | Check whether a node has an outgoing transition for the given byte.
hasTransition :: BuildEnv -> NodeId -> Word8 -> IO Bool
hasTransition env s t
    | s == envBottomId env = pure True
    | otherwise = do
        nd <- lookupNode env s
        pure $! IMap.member (fromIntegral t) (children nd)

-- | Look up the transition selected by the byte at text index @k@.
lookupTransition :: BuildEnv -> NodeId -> Int -> IO Edge
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
findTkTransition :: BuildEnv -> NodeId -> Int -> Int -> IO IntTriple
findTkTransition env s k maxIdx = do
    edge <- lookupTransition env s k
    let edgeEnd = resolveEdgeEnd maxIdx (end edge)
    pure (IT (target edge) (start edge) edgeEnd)

-- | Follow a node's suffix link.
nodeSuffixLink :: BuildEnv -> NodeId -> IO NodeId
nodeSuffixLink env nid = do
    nd <- lookupNode env nid
    case suffixLink nd of
        Just link -> pure link
        Nothing   -> error "Missing suffix link"

-- | Canonicalise an active point to the closest explicit node or edge start.
canonize :: BuildEnv -> NodeId -> (Int, Int) -> Int -> IO IntPair
canonize env s (k, i) maxIdx
    | i < k     = pure $! IP s k
    | otherwise = do
        res <- findTkTransition env s k maxIdx
        go (IT s k i) res
  where
    go :: IntTriple -> IntTriple -> IO IntPair
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
testAndSplit :: BuildEnv -> NodeId -> (Int, Int) -> Word8 -> IO (Bool, NodeId)
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
addLeaf :: BuildEnv -> NodeId -> Int -> IO ()
addLeaf env parent startIdx = do
    leafId <- newNode env
    let edge = Edge { start = startIdx, end = SNothing, target = leafId }
        firstChar = B.index (envText env) startIdx
    setTransition env parent firstChar edge

-- | Run Ukkonen's update step for one input position.
update :: BuildEnv -> Int -> IO (NodeId, Int)
update env maxIdx = do
    s <- readIORef (envActiveS env)
    k <- readIORef (envActiveK env)
    let currentChar = B.index (envText env) maxIdx
    (endPoint, r) <- testAndSplit env s (k, maxIdx - 1) currentChar
    go currentChar (envRootId env) s k endPoint r
  where
    go :: Word8 -> NodeId -> NodeId -> Int -> Bool -> NodeId -> IO (NodeId, Int)
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
extendAtIndex :: BuildEnv -> Int -> IO ()
extendAtIndex env i = do
    (s', k') <- update env i
    (IP s'' k'') <- canonize env s' (k', i) i
    writeIORef (envActiveS env) s''
    writeIORef (envActiveK env) k''

-- | Build the tree in IO and freeze the hash table to an immutable 'IntMap'.
buildAndFreeze :: InputText -> IO STree
buildAndFreeze input = do
    env <- initBuildEnv input
    let lastIdx = B.length input - 1
        loop i
          | i > lastIdx = pure ()
          | otherwise   = extendAtIndex env i >> loop (i + 1)
    loop 0
    pairs <- H.toList (envNodes env)
    pure STree
      { text     = input
      , nodes    = IMap.fromList pairs
      , rootId   = envRootId env
      , bottomId = envBottomId env
      }

-- | Build a suffix tree containing every suffix of the input.
--
-- Construction uses a mutable hash table internally; the result is frozen to
-- a persistent 'IntMap' so that queries remain pure.
sTree :: InputText -> STree
sTree input = unsafePerformIO (buildAndFreeze input)
{-# NOINLINE sTree #-}

-- | Check whether a pattern occurs in the indexed text.
containsString :: STree -> InputText -> Bool
containsString _ patternText
    | B.null patternText = True
containsString tree patternText = goNode (rootId tree) 0
  where
    patternLen = B.length patternText
    textLen    = B.length (text tree)

    goNode currentNode patternIdx
        | patternIdx >= patternLen = True
        | otherwise =
            case IMap.lookup currentNode (nodes tree) of
                Nothing -> False
                Just current ->
                    let key = fromIntegral (B.index patternText patternIdx)
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