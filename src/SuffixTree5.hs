{-# LANGUAGE ScopedTypeVariables #-}

-- | Ukkonen-style suffix tree over strict 'ByteString' values.
--
-- Construction uses a pre-allocated mutable boxed vector ('Data.Vector.Mutable')
-- for O(1) worst-case node operations within the 'ST' monad.
-- A suffix tree for an input of length n contains at most
-- 2n - 1 tree nodes; adding the bottom and root sentinels gives a maximum of
-- 2n + 1 entries, so the vector is sized at @2 * n + 2@ up front and never
-- resized.
--
-- Edge maps is an 'IntMap'-keyed by 'Word8' values (bounded at 256 entries).
--
-- After construction the mutable vector is sliced to the live region
-- @[0 .. nextId - 1]@ and frozen in O(1) via 'V.unsafeFreeze', producing an
-- immutable boxed 'Vector' for pure O(1) queries.
module SuffixTree5 (
    STree (..),
    Edge (..),
    Node (..),
    sTree,
    containsString,
    findOccurrences
) where

import qualified Data.ByteString as B
import qualified Data.IntMap.Strict as IMap
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Control.Monad (when)
import           Control.Monad.ST
import           Data.STRef
import           Data.Word (Word8)


type NodeId = Int

data Symbol = Byte !Word8 | End deriving (Show, Eq)

symbolKey :: Symbol -> Int
symbolKey (Byte w) = fromIntegral w
symbolKey End      = 256

symbolAt :: B.ByteString -> Int -> Symbol
symbolAt bs i
  | i < B.length bs = Byte (B.index bs i)
  | i == B.length bs = End
  | otherwise = error "symbolAt: out of bounds"

-- | Strict pair of 'Int' values to prevent heap allocation
data IntPair = IP {-# UNPACK #-} !Int
                  {-# UNPACK #-} !Int

-- | Strict triple of 'Int' values to prevent heap allocation
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
  { text     :: !B.ByteString
  , nodes    :: !(V.Vector Node)
  , rootId   :: {-# UNPACK #-} !NodeId
  , bottomId :: {-# UNPACK #-} !NodeId
  } deriving Show

-- | Mutable construction environment parameterised by the 'ST' state token.
--
-- All node storage goes through 'envNodes', a pre-allocated mutable vector
-- with O(1) worst-case read and write.  Scalar state (next id, active point)
-- lives in 'STRef's for in-place update.
data BuildEnv s = BuildEnv
  { envNodes    :: !(MV.MVector s Node)
  , envText     :: !B.ByteString
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
--
-- The vector is pre-sized to @2 * n + 2@ where @n@ is the input length.
-- A suffix tree contains at most @2n - 1@ tree nodes (Lemma nodeCount),
-- plus the bottom and root sentinels, giving @2n + 1@ entries.  The extra
-- slot avoids an off-by-one for empty input.
initBuildEnv :: B.ByteString -> ST s (BuildEnv s)
initBuildEnv input = do
    let n   = B.length input
        bId = 0
        rId = 1
    mv <- MV.new (2 * n + 4) -- 2 * (n + 1) + 2 
    MV.write mv bId emptyNode
    MV.write mv rId (mkNode bId)
    nRef <- newSTRef 2
    sRef <- newSTRef rId
    kRef <- newSTRef 0
    pure BuildEnv
      { envNodes    = mv
      , envText     = input
      , envRootId   = rId
      , envBottomId = bId
      , envNextId   = nRef
      , envActiveS  = sRef
      , envActiveK  = kRef
      }

-- | Allocate a fresh node id and write an empty node at that index.
newNode :: BuildEnv s -> ST s NodeId
newNode env = do
    nid <- readSTRef (envNextId env)
    writeSTRef (envNextId env) $! nid + 1
    MV.write (envNodes env) nid emptyNode
    pure nid

-- | Read a node by id.  O(1) worst-case vector index.
readNode :: BuildEnv s -> NodeId -> ST s Node
readNode env nid = MV.read (envNodes env) nid

-- | Set an outgoing edge for a byte transition on a node.
setTransition :: BuildEnv s -> NodeId -> Symbol -> Edge -> ST s ()
setTransition env n t edge = do
    nd <- readNode env n
    let nd' = nd { children = IMap.insert (symbolKey t) edge (children nd) }
    MV.write (envNodes env) n nd'

-- | Set or replace the suffix link for a node created during a split.
setSuffixLink :: BuildEnv s -> NodeId -> NodeId -> ST s ()
setSuffixLink env n tid = do
    nd <- readNode env n
    MV.write (envNodes env) n (nd { suffixLink = SJust tid })

-- | Check whether a node has an outgoing transition for the given byte.
hasTransition :: BuildEnv s -> NodeId -> Symbol -> ST s Bool
hasTransition env s t
    | s == envBottomId env = pure True
    | otherwise = do
        nd <- readNode env s
        pure $! IMap.member (symbolKey t) (children nd)

-- | Look up the transition selected by the byte at text index @k@.
lookupTransition :: BuildEnv s -> NodeId -> Int -> ST s Edge
lookupTransition env s k
    | s == envBottomId env =
        pure Edge { start = k, end = SJust k, target = envRootId env }
    | otherwise = do
        snode <- readNode env s
        let key = symbolKey $ symbolAt (envText env) k
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
    nd <- readNode env nid
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
testAndSplit :: BuildEnv s -> NodeId -> (Int, Int) -> Symbol -> ST s (Bool, NodeId)
testAndSplit env s (k, p) t
    | k <= p = do
        edge <- lookupTransition env s k
        let k'       = start edge
            splitEnd = k' + p - k
            nextChar = symbolAt (envText env) (splitEnd + 1)
        if t == nextChar
            then pure (True, s)
            else do
                r <- newNode env
                let prefixEdge = Edge { start = k', end = SJust splitEnd, target = r }
                    suffixEdge = Edge { start = splitEnd + 1, end = end edge, target = target edge }
                setTransition env s (symbolAt (envText env) k') prefixEdge
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
        firstSymbol = symbolAt (envText env) startIdx -- B.index (envText env) startIdx
    setTransition env parent firstSymbol edge

-- | Run Ukkonen's update step for one input position.
update :: forall s. BuildEnv s -> Int -> ST s (NodeId, Int)
update env maxIdx = do
    s <- readSTRef (envActiveS env)
    k <- readSTRef (envActiveK env)
    let currentSymbol = symbolAt (envText env) maxIdx -- B.index (envText env) maxIdx
    (endPoint, r) <- testAndSplit env s (k, maxIdx - 1) currentSymbol
    go currentSymbol (envRootId env) s k endPoint r
  where
    go :: Symbol -> NodeId -> NodeId -> Int -> Bool -> NodeId -> ST s (NodeId, Int)
    go currentSymbol oldr currentS currentK endPoint r
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
            (endPoint', r') <- testAndSplit env canonS (canonK, maxIdx - 1) currentSymbol
            go currentSymbol r canonS canonK endPoint' r'

-- | Extend the tree so it represents suffixes ending at the given input index.
extendAtIndex :: BuildEnv s -> Int -> ST s ()
extendAtIndex env i = do
    (s', k') <- update env i
    (IP s'' k'') <- canonize env s' (k', i) i
    writeSTRef (envActiveS env) s''
    writeSTRef (envActiveK env) k''

-- | Build the tree in 'ST' and freeze the mutable vector into an immutable one.
--
-- Node ids are sequential integers from 0, so the live entries occupy
-- indices @[0 .. nextId - 1]@.  'MV.slice' produces a zero-copy view of
-- this region, and 'V.unsafeFreeze' converts it to an immutable vector in
-- O(1) since the mutable vector is never touched again.
buildAndFreeze :: B.ByteString -> ST s STree
buildAndFreeze input = do
    env <- initBuildEnv input
    let lastIdx = B.length input
        loop i
          | i > lastIdx = pure ()
          | otherwise   = extendAtIndex env i >> loop (i + 1)
    loop 0
    -- Freeze: slice to live region, then freeze in O(1)
    nodeCount <- readSTRef (envNextId env)
    let liveSlice = MV.slice 0 nodeCount (envNodes env)
    frozen <- V.unsafeFreeze liveSlice
    pure STree
      { text     = input
      , nodes    = frozen
      , rootId   = envRootId env
      , bottomId = envBottomId env
      }

-- | Build a suffix tree containing every suffix of the input.
--
-- Construction uses a pre-allocated mutable vector internally via the 'ST'
-- monad; the result is frozen to an immutable 'Vector' in O(1) so that
-- queries remain pure with O(1) node access.
sTree :: B.ByteString -> STree
sTree input = runST (buildAndFreeze input)

-- | Check whether a pattern occurs in the indexed text.
containsString :: STree -> B.ByteString -> Bool
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

    edgeStop :: Edge -> Int
    edgeStop edge = resolveEdgeEnd (textLen - 1) (end edge)

-- | Find all starting positions where the pattern occurs in the indexed text.
--
-- Walks the tree in O(m) to locate the match point, then collects every leaf
-- in the subtree below it.  Each leaf corresponds to a suffix whose start
-- index is an occurrence position.  The total cost is O(m + occ) where occ
-- is the number of occurrences.
findOccurrences :: STree -> B.ByteString -> [Int]
findOccurrences _ patternText
    | B.null patternText = []
findOccurrences tree patternText = goNode (rootId tree) 0 0
  where
    patternLen = B.length patternText
    textLen    = B.length (text tree)
    nodeVec    = nodes tree

    -- Walk from an explicit node, matching pattern characters.
    -- @depth@ is the total number of text characters on the path from root
    -- to this node.
    goNode :: NodeId -> Int -> Int -> [Int]
    goNode currentNode patternIdx depth
        | patternIdx >= patternLen = collectLeaves currentNode depth
        | otherwise =
            let current = nodeVec V.! currentNode
                key     = fromIntegral (B.index patternText patternIdx)
            in case IMap.lookup key (children current) of
                Nothing   -> []
                Just edge -> goEdge edge patternIdx depth (start edge)

    -- Walk along an edge, matching pattern characters.
    goEdge :: Edge -> Int -> Int -> Int -> [Int]
    goEdge edge patternIdx depth textIdx
        | patternIdx >= patternLen =
            let depthAtTarget = depth + edgeStop edge - start edge + 1
            in collectLeaves (target edge) depthAtTarget
        | textIdx > edgeStop edge =
            let depthAtTarget = depth + edgeStop edge - start edge + 1
            in goNode (target edge) patternIdx depthAtTarget
        | textIdx >= textLen = []
        | B.index patternText patternIdx == B.index (text tree) textIdx =
            goEdge edge (patternIdx + 1) depth (textIdx + 1)
        | otherwise = []

    -- Collect the starting index of every suffix reachable from a node.
    -- A node with no children is a leaf; its suffix starts at @textLen - depth@.
    collectLeaves :: NodeId -> Int -> [Int]
    collectLeaves nodeId depth =
        let node = nodeVec V.! nodeId
        in if IMap.null (children node)
           then [textLen - depth]
           else concatMap (\edge ->
                    let edgeLen = edgeStop edge - start edge + 1
                    in collectLeaves (target edge) (depth + edgeLen)
                ) (IMap.elems (children node))

    edgeStop :: Edge -> Int
    edgeStop edge = resolveEdgeEnd (textLen - 1) (end edge)