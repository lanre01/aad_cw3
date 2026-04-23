module SuffixTree3 (
    STree (..),
    Edge (..),
    Node (..),
    sTree,
    containsString
) where 

import qualified Data.ByteString as B
import qualified Data.IntMap.Strict as IMap
import           Data.Word (Word8)
import           Control.Monad.State 
import           Control.Monad
-- (State, runState)

type InputText = B.ByteString
type NodeId = Int

data Edge = Edge
  { start  :: !Int
  , end    :: !(Maybe Int)
  , target :: !NodeId
  } deriving Show

data Node = Node
  { suffixLink :: !(Maybe NodeId)
  , children   :: !(IMap.IntMap Edge)
  } deriving Show

data STree = STree
  { text     :: !InputText
  , nodes    :: !(IMap.IntMap Node)
  , rootId   :: !NodeId
  , bottomId :: !NodeId
  } deriving Show

data BuildState = BuildState
  { buildTree :: !STree
  , nextId    :: !NodeId
  , activeS   :: !NodeId
  , activeK   :: !Int
  }

emptyNode :: Node 
emptyNode = Node {suffixLink = Nothing, children = IMap.empty}

node :: NodeId -> Node 
node suffixId = Node {suffixLink = Just suffixId, children = IMap.empty}

initNodes :: NodeId -> NodeId -> IMap.IntMap Node
initNodes bottomIdx rootIdx = 
    let rootNode = node bottomIdx
        bottomNode = emptyNode
    in  IMap.fromList [(rootIdx, rootNode), (bottomIdx, bottomNode)]

initSTree :: InputText -> STree 
initSTree input = STree {text = input, nodes = nodes', rootId = 1, bottomId = 0}
           where 
           nodes'  = initNodes 0 1


type BuildS = State BuildState

initBuildState :: InputText -> BuildState
initBuildState input =
    let tree = initSTree input
    in BuildState
        { buildTree = tree
        , nextId    = rootId tree + 1
        , activeS   = rootId tree
        , activeK   = 0
        }

getRootId :: BuildS NodeId
getRootId = gets (rootId . buildTree)

getBottomId :: BuildS NodeId
getBottomId = gets (bottomId . buildTree)

getActiveS :: BuildS NodeId 
getActiveS = gets activeS 

getActiveK :: BuildS Int  
getActiveK = gets activeK 

getTree :: BuildS STree 
getTree = gets buildTree 

setActiveS :: NodeId -> BuildS ()
setActiveS sid = modify' (\st -> st {activeS = sid})

setActiveK :: Int -> BuildS ()
setActiveK k = modify' (\st -> st {activeK = k})

putTree :: STree -> BuildS ()
putTree t = modify' $ \st -> st { buildTree = t }

modifyTree :: (STree -> STree) -> BuildS ()
modifyTree f = modify' $ \st -> st { buildTree = f (buildTree st) }

freshId :: BuildS NodeId
freshId = do
  nid <- gets nextId
  modify' (\st -> st { nextId = nid + 1 })
  pure nid

newNode :: BuildS NodeId
newNode = do
    nid <- freshId 
    modifyTree (\tree -> tree {nodes = IMap.insert nid emptyNode (nodes tree)} )
    pure nid

sTree :: InputText -> STree
sTree input =
    buildTree $ execState (go 0) (initBuildState input)
  where
    lastIdx = B.length input - 1

    go :: Int -> BuildS ()
    go i | i > lastIdx =  pure () 
         | otherwise = do 
                     extendAtIndex i
                     go (i+1)

extendAtIndex :: Int -> BuildS ()
extendAtIndex i = do 
    s <- getActiveS  -- actuall could avoid getting the s and k here to make it cleaner by
    k <- getActiveK  -- just setting s and k in update and canonize
    (s', k') <- update s k i 
    (s'', k'') <- canonize s' (k', i) i 
    modify' $ \st' -> st' {activeS = s'', activeK = k''}


update :: NodeId -> Int -> Int -> BuildS (NodeId, Int)
update s k maxIdx = do
    tree <- getTree
    let currentChar = B.index (text tree) maxIdx
    (endPoint, r) <- testAndSplit s (k, maxIdx - 1) currentChar
    go currentChar (rootId tree) s k endPoint r
  where
    go currentChar oldr currentS currentK endPoint r
        | endPoint = do
            tree <- getTree
            when (oldr /= rootId tree) $
                setSuffixLink oldr currentS
            pure (currentS, currentK)
        | otherwise = do
            _ <- addLeaf r maxIdx
            tree <- getTree
            when (oldr /= rootId tree) $
                setSuffixLink oldr r
            nextS <- nodeSuffixLink currentS
            (canonS, canonK) <- canonize nextS (currentK, maxIdx - 1) maxIdx
            (endPoint', r') <- testAndSplit canonS (canonK, maxIdx - 1) currentChar
            go currentChar r canonS canonK endPoint' r'

setSuffixLink :: NodeId -> NodeId -> BuildS ()
setSuffixLink n targetId =
    modifyTree $ \tree ->
        tree { nodes = IMap.adjust (\nd -> nd { suffixLink = Just targetId }) n (nodes tree) }

addLeaf :: NodeId -> Int -> BuildS NodeId 
addLeaf parent startIdx = do 
    tree <- getTree
    leafId <- newNode
    let edge = Edge { start = startIdx, end = Nothing, target = leafId }
        firstChar = B.index (text tree) startIdx
    setTransition parent firstChar edge 
    pure leafId 

nodeSuffixLink :: NodeId -> BuildS NodeId 
nodeSuffixLink nid = do 
    n <- lookupNode nid
    case suffixLink n of 
        Just link -> pure link 
        Nothing   -> error "Missing suffix link, wierd"

canonize :: NodeId -> (Int, Int) -> Int -> BuildS (NodeId, Int)
canonize s (k, i) maxIdx | i < k     = pure (s, k)
                         | otherwise = do 
                             res <- findTkTransition s k maxIdx
                             go (s, k, i) res 
                  where go :: (NodeId, Int, Int) -> (NodeId, Int, Int) -> BuildS (NodeId, Int)
                        go (s, k, p) (s', k', p') 
                            | p' - k' <= p - k = 
                                    do 
                                let k'' = k + p' - k' + 1
                                if k'' <= p 
                                    then do 
                                    res <- findTkTransition s' k'' maxIdx 
                                    go (s', k'', p) res
                                else pure (s', k'')
                            | otherwise = pure (s, k)
                    
                             
findTkTransition :: NodeId -> Int -> Int -> BuildS (NodeId, Int, Int)                           
findTkTransition s k maxIdx = do 
    edge <- lookupTransition s k 
    let edgeEnd = maybe maxIdx id (end edge)
    pure (target edge, start edge, edgeEnd)

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

testAndSplit :: NodeId -> (Int, Int) -> Word8 -> BuildS (Bool, NodeId)
testAndSplit s (k, p) t
    | k <= p = do
        tree <- getTree
        edge <- lookupTransition s k
        let k' = start edge
            splitEnd = k' + p - k
            nextChar = B.index (text tree) (splitEnd + 1)
        if t == nextChar
           then pure (True, s)
           else do
               r <- newNode
               let prefixEdge = Edge { start = k', end = Just splitEnd, target = r }
                   suffixEdge = Edge { start = splitEnd + 1, end = end edge, target = target edge }
               setTransition s (B.index (text tree) k') prefixEdge
               setTransition r nextChar suffixEdge
               pure (False, r)
    | otherwise = do
        exists <- hasTransition s t
        pure (exists, s)

hasTransition :: NodeId -> Word8 -> BuildS Bool
hasTransition s t = do 
   bid <- getBottomId
   if s == bid then pure True
   else  do 
    n <- lookupNode s 
    pure (IMap.member (fromIntegral t) (children n))

    

lookupNode :: NodeId -> BuildS Node  
lookupNode nid = do
    nodes <- fmap nodes getTree
    case IMap.lookup nid nodes of 
        Just n -> pure n 
        Nothing   -> error "Node not present, wierd" 

lookupTransition :: NodeId -> Int -> BuildS Edge
lookupTransition s k
    = do
        tree <- getTree
        if s == bottomId tree
           then pure Edge { start = k, end = Just k, target = rootId tree }
           else case IMap.lookup s (nodes tree) of
                  Nothing -> error "Node not present, weird"
                  Just snode ->
                      let key = fromIntegral $ B.index (text tree) k
                      in case IMap.lookup key (children snode) of
                           Nothing   -> error "Edge not present, weird"
                           Just edge -> pure edge


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
                        Nothing -> False
                        Just edge -> goEdge edge patternIdx (start edge)

    goEdge edge patternIdx textIdx
        | patternIdx >= patternLen = True
        | textIdx > edgeStop edge = goNode (target edge) patternIdx
        | textIdx >= textLen = False
        | B.index patternText patternIdx == B.index (text tree) textIdx =
            goEdge edge (patternIdx + 1) (textIdx + 1)
        | otherwise = False

    edgeStop edge = maybe (textLen - 1) id (end edge)
