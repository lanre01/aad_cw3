-- | Ukkonen-style suffix tree over strict 'ByteString' values.
--
-- This fourth implementation keeps the same external shape as the earlier
-- suffix-tree modules, but it tightens a few internal representations used in
-- the hot construction path. Small multi-value results are represented with
-- strict single-constructor records whose 'Int' fields can be unpacked by GHC,
-- avoiding allocation of ordinary lazy tuples in repeated canonicalisation
-- steps. Edge ends also use a strict specialised optional 'Int', so open leaf
-- edges can share the current maximum input index without allocating
-- 'Maybe Int' values.
module SuffixTree4 (
    STree (..),
    Edge (..),
    Node (..),
    sTree,
    containsString
) where

import qualified Data.ByteString as B
import qualified Data.IntMap.Strict as IMap
import           Control.Monad (when)
import           Control.Monad.State
import           Data.Word (Word8)

type InputText = B.ByteString

type NodeId = Int

-- | Strict pair of 'Int' values used as an allocation-conscious return type.
--
-- 'canonize' is called for every input position while building the tree. Using
-- this unpackable constructor avoids the boxed, lazy fields of a normal
-- @(Int, Int)@ result in that performance-sensitive loop.
data IntPair = IP {-# UNPACK #-} !Int 
                  {-# UNPACK #-} !Int

-- | Strict triple of 'Int' values used while walking edge intervals.
--
-- The constructor stores the target node, edge start, and resolved edge end as
-- unpacked integers. This keeps transition metadata compact while
-- canonicalising active points.
data IntTriple = IT {-# UNPACK #-} !Int 
                    {-# UNPACK #-} !Int 
                    {-# UNPACK #-} !Int 

-- | Specialised strict optional edge end.
--
-- 'SNothing' marks a leaf edge whose end is the current last processed input
-- index. 'SJust' stores a fixed internal-edge end index. This mirrors the role
-- of @Maybe Int@ in the earlier implementations, but avoids a boxed 'Int' in
-- the hot edge representation.
data StrictIntMaybe
  = SNothing
  | SJust {-# UNPACK #-} !Int deriving Show 


-- | Directed edge labelled by a slice of the indexed input.
--
-- The label is not copied out of the 'ByteString'. Instead, @start@ and @end@
-- identify an inclusive interval in 'text'. A missing end means the edge is a
-- leaf whose end grows as construction advances, which is the usual Ukkonen
-- trick for updating all leaves in constant time.
data Edge = Edge
  { start  :: {-# UNPACK #-} !Int
  , end    :: !StrictIntMaybe
  , target :: {-# UNPACK #-} !Int
  } deriving Show

-- | Tree node with its suffix link and outgoing byte transitions.
--
-- Children are stored in a strict 'IMap.IntMap' keyed by the next byte value.
-- The suffix link is absent only for nodes that do not yet have one assigned
-- during construction.
data Node = Node
  { suffixLink :: !(Maybe NodeId)
  , children   :: !(IMap.IntMap Edge)
  } deriving Show

-- | Complete suffix tree plus the immutable text it indexes.
--
-- Edges refer back into @text@ by integer bounds, so the tree stores only
-- indices and nodes rather than duplicating substrings. The root and bottom
-- node ids are cached because they are consulted repeatedly in Ukkonen's
-- transition rules.
data STree = STree
  { text     :: !InputText
  , nodes    :: !(IMap.IntMap Node)
  , rootId   :: {-# UNPACK #-} !NodeId
  , bottomId :: {-# UNPACK #-} !NodeId
  } deriving Show

-- | Mutable construction state threaded through the strict state monad.
--
-- @activeS@ and @activeK@ are the active point used by Ukkonen's algorithm.
-- Keeping these fields strict and unpacked avoids retaining old state records
-- as the builder advances through the input.
data BuildState = BuildState
  { buildTree :: !STree
  , nextId    :: {-# UNPACK #-} !NodeId
  , activeS   :: {-# UNPACK #-} !NodeId
  , activeK   :: {-# UNPACK #-} !Int
  }

type BuildS = State BuildState

emptyNode :: Node
emptyNode = Node { suffixLink = Nothing, children = IMap.empty }

-- | Create an empty node with a known suffix link.
node :: NodeId -> Node
node suffixId = Node { suffixLink = Just suffixId, children = IMap.empty }

-- | Create the initial root and bottom nodes used by Ukkonen's algorithm.
--
-- The root starts with a suffix link to the artificial bottom node. Bottom-node
-- transitions are handled specially by 'lookupTransition' and are not stored
-- explicitly in the map.
initNodes :: NodeId -> NodeId -> IMap.IntMap Node
initNodes bottomIdx rootIdx =
    let rootNode   = node bottomIdx
        bottomNode = emptyNode
    in IMap.fromList [(rootIdx, rootNode), (bottomIdx, bottomNode)]

-- | Create the initial empty tree structure for an input.
initSTree :: InputText -> STree
initSTree input =
    STree
      { text = input
      , nodes = initNodes 0 1
      , rootId = 1
      , bottomId = 0
      }

-- | Initialise the construction state before the first extension.
initBuildState :: InputText -> BuildState
initBuildState input =
    let tree = initSTree input
    in BuildState
        { buildTree = tree
        , nextId    = rootId tree + 1
        , activeS   = rootId tree
        , activeK   = 0
        }

getTree :: BuildS STree
getTree = gets buildTree

getBottomId :: BuildS NodeId
getBottomId = gets (bottomId . buildTree)

getActivePoint :: BuildS (NodeId, Int)
getActivePoint = gets (\st -> (activeS st, activeK st))

setActivePoint :: NodeId -> Int -> BuildS ()
setActivePoint s k =
    modify' $ \st -> st { activeS = s, activeK = k }

modifyTree :: (STree -> STree) -> BuildS ()
modifyTree f =
    modify' $ \st -> st { buildTree = f (buildTree st) }

freshId :: BuildS NodeId
freshId = state $ \st ->
    let nid = nextId st
    in (nid, st { nextId = nid + 1 })

-- | Insert a new empty node and return its freshly allocated id.
newNode :: BuildS NodeId
newNode = do
    nid <- freshId
    modifyTree $ \tree ->
        tree { nodes = IMap.insert nid emptyNode (nodes tree) }
    pure nid

-- | Build a suffix tree containing every suffix of the input.
--
-- The builder processes the input from left to right. Leaf edges keep
-- 'SNothing' as their end value, so their effective end is resolved against the
-- current index rather than updated one by one after each extension.
sTree :: InputText -> STree
sTree input =
    buildTree $ execState (go 0) (initBuildState input)
  where
    lastIdx = B.length input - 1

    go :: Int -> BuildS ()
    go i
      | i > lastIdx = pure ()
      | otherwise = do
          extendAtIndex i
          go (i + 1)

-- | Extend the tree so it represents suffixes ending at the given input index.
extendAtIndex :: Int -> BuildS ()
extendAtIndex i = do
    (s', k')   <- update i
    (IP s'' k'') <- canonize s' (k', i) i
    setActivePoint s'' k''

-- | Run Ukkonen's update step for one input position.
--
-- The function repeatedly tests the active point, adds a leaf or splits an
-- edge when needed, follows suffix links, and then canonicalises the next
-- active point. It returns the active point for the caller to store.
update :: Int -> BuildS (NodeId, Int)
update maxIdx = do
    (s, k) <- getActivePoint
    tree <- getTree
    let currentChar = B.index (text tree) maxIdx
    (endPoint, r) <- testAndSplit s (k, maxIdx - 1) currentChar
    go currentChar (rootId tree) s k endPoint r
  where
    go :: Word8 -> NodeId -> NodeId -> Int -> Bool -> NodeId -> BuildS (NodeId, Int)
    go currentChar oldr currentS currentK endPoint r
        | endPoint = do
            tree <- getTree
            when (oldr /= rootId tree) $
                setSuffixLink oldr currentS
            pure (currentS, currentK)

        | otherwise = do
            addLeaf r maxIdx
            tree <- getTree
            when (oldr /= rootId tree) $
                setSuffixLink oldr r
            nextS <- nodeSuffixLink currentS
            (IP canonS canonK) <- canonize nextS (currentK, maxIdx - 1) maxIdx
            (endPoint', r')  <- testAndSplit canonS (canonK, maxIdx - 1) currentChar
            go currentChar r canonS canonK endPoint' r'

-- | Set or replace the suffix link for a node created during a split.
setSuffixLink :: NodeId -> NodeId -> BuildS ()
setSuffixLink n targetId =
    modifyTree $ \tree ->
        tree
          { nodes =
              IMap.adjust
                (\nd -> nd { suffixLink = Just targetId })
                n
                (nodes tree)
          }

-- | Add a leaf edge for the suffix beginning at the given index.
--
-- The edge end is stored as 'SNothing', meaning it remains open and resolves to
-- the latest processed input index.
addLeaf :: NodeId -> Int -> BuildS ()
addLeaf parent startIdx = do
    tree <- getTree
    leafId <- newNode
    let edge = Edge { start = startIdx, end = SNothing, target = leafId }
        firstChar = B.index (text tree) startIdx
    setTransition parent firstChar edge

-- | Follow a node's suffix link, failing if the construction invariant is broken.
nodeSuffixLink :: NodeId -> BuildS NodeId
nodeSuffixLink nid = do
    n <- lookupNode nid
    case suffixLink n of
        Just link -> pure link
        Nothing   -> error "Missing suffix link"

-- | Canonicalise an active point to the closest explicit node or edge start.
--
-- Ukkonen's algorithm may produce an active point whose path crosses complete
-- edges. This walks those edges until @(s, k)@ is canonical. The return value
-- uses 'IntPair' rather than a normal tuple because this function sits in the
-- tight construction loop.
canonize :: NodeId -> (Int, Int) -> Int -> BuildS IntPair
canonize s (k, i) maxIdx
    | i < k = pure $! IP s k
    | otherwise = do
        res <- findTkTransition s k maxIdx
        go (IT s k i) res
  where
    go :: IntTriple -> IntTriple -> BuildS IntPair
    go (IT s0 k0 p0) (IT s1 k1 p1)
        | p1 - k1 <= p0 - k0 = do
            let k2 = k0 + p1 - k1 + 1
            if k2 <= p0
                then do
                    tkTrans <- findTkTransition s1 k2 maxIdx
                    go (IT s1 k2 p0) tkTrans
                else pure $! IP s1 k2 
        | otherwise = pure $! IP s0 k0 

-- | Return the transition target and concrete label bounds for an active point.
--
-- Open leaf ends are resolved against @maxIdx@ here, giving callers a concrete
-- inclusive interval without changing the stored edge.
findTkTransition :: NodeId -> Int -> Int -> BuildS IntTriple
findTkTransition s k maxIdx = do
    edge <- lookupTransition s k
    let edgeEnd = resolveEdgeEnd maxIdx (end edge)
    pure (IT (target edge) (start edge) edgeEnd)

-- | Resolve a stored edge end to a concrete inclusive index.
--
-- 'SNothing' means an open leaf edge, so the supplied current maximum index is
-- used. 'SJust' stores a fixed end for an internal edge and ignores that
-- fallback value.
resolveEdgeEnd :: Int -> StrictIntMaybe -> Int
resolveEdgeEnd currentMax SNothing  = currentMax
resolveEdgeEnd _          (SJust a) = a

-- | Set an outgoing edge for a byte transition on a node.
setTransition :: NodeId -> Word8 -> Edge -> BuildS ()
setTransition n t edge =
    modifyTree $ \tree ->
        tree
          { nodes =
              IMap.adjust
                (\nd -> nd { children = IMap.insert (fromIntegral t) edge (children nd) })
                n
                (nodes tree)
          }

-- | Test whether the active path already continues with a byte or needs a split.
--
-- When @(k, p)@ names a non-empty path, the function compares the next byte on
-- the matching edge with the requested transition. A mismatch creates a new
-- internal node and replaces the original edge with prefix and suffix edges.
testAndSplit :: NodeId -> (Int, Int) -> Word8 -> BuildS (Bool, NodeId)
testAndSplit s (k, p) t
    | k <= p = do
        tree <- getTree
        edge <- lookupTransition s k
        let k'       = start edge
            splitEnd = k' + p - k
            nextChar = B.index (text tree) (splitEnd + 1)
        if t == nextChar
            then pure (True, s)
            else do
                r <- newNode
                let prefixEdge = Edge { start = k', end = SJust splitEnd, target = r }
                    suffixEdge = Edge { start = splitEnd + 1, end = end edge, target = target edge }
                setTransition s (B.index (text tree) k') prefixEdge
                setTransition r nextChar suffixEdge
                pure (False, r)

    | otherwise = do
        exists <- hasTransition s t
        pure (exists, s)

-- | Check whether a node has an outgoing transition for the given byte.
--
-- The artificial bottom node is treated as having every transition, matching
-- the standard suffix-tree construction rule.
hasTransition :: NodeId -> Word8 -> BuildS Bool
hasTransition s t = do
    bid <- getBottomId
    if s == bid
        then pure True
        else do
            n <- lookupNode s
            pure (IMap.member (fromIntegral t) (children n))

-- | Look up a node by id, failing if the internal node map is inconsistent.
lookupNode :: NodeId -> BuildS Node
lookupNode nid = do
    ns <- fmap nodes getTree
    case IMap.lookup nid ns of
        Just n  -> pure n
        Nothing -> error "Node not present"

-- | Look up the transition selected by the byte at text index @k@.
--
-- For the artificial bottom node, the transition is generated on demand: it
-- consumes exactly the byte at @k@ and targets the root. Other nodes consult
-- their child map using the indexed byte as the key.
lookupTransition :: NodeId -> Int -> BuildS Edge
lookupTransition s k = do
    tree <- getTree
    if s == bottomId tree
        then pure Edge { start = k, end = SJust k, target = rootId tree }
        else case IMap.lookup s (nodes tree) of
            Nothing -> error "Node not present"
            Just snode ->
                let key = fromIntegral $ B.index (text tree) k
                in case IMap.lookup key (children snode) of
                    Nothing   -> error "Edge not present"
                    Just edge -> pure edge

-- | Check whether a pattern occurs in the indexed text.
--
-- This walks the already-built tree without allocating substrings. Edge labels
-- are compared directly against the original 'ByteString' by index, resolving
-- open leaf ends against the final text length.
containsString :: STree -> InputText -> Bool
containsString _ patternText
    | B.null patternText = True
containsString tree patternText = goNode (rootId tree) 0
  where
    patternLen = B.length patternText
    textLen = B.length (text tree)

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
