module SuffixTree3 (
    STree3 (..),
    Edge3 (..),
    Node3 (..),
    sTree3,
    containsString3
) where 

import qualified Data.ByteString as B
import qualified Data.IntMap.Strict as IMap
import           Data.Word (Word8)
import           Control.Monad.State 
import           Control.Monad
-- (State, runState)

type InputText = B.ByteString
type NodeId = Int

data Edge3 = Edge3
  { start3  :: !Int
  , end3    :: !(Maybe Int)
  , target3 :: !NodeId
  } deriving Show

data Node3 = Node3
  { suffixLink3 :: !(Maybe NodeId)
  , children3   :: !(IMap.IntMap Edge3)
  } deriving Show

data STree3 = STree3
  { text3     :: !InputText
  , nodes3    :: !(IMap.IntMap Node3)
  , rootId3   :: !NodeId
  , bottomId3 :: !NodeId
  } deriving Show

data BuildState = BuildState
  { buildTree :: !STree3
  , nextId    :: !NodeId
  , activeS   :: !NodeId
  , activeK   :: !Int
  }

emptyNode :: Node3 
emptyNode = Node3 {suffixLink3 = Nothing, children3 = IMap.empty}

node :: NodeId -> Node3 
node suffixId = Node3 {suffixLink3 = Just suffixId, children3 = IMap.empty}

initNodes :: NodeId -> NodeId -> IMap.IntMap Node3
initNodes bottomIdx rootIdx = 
    let rootNode = node bottomIdx
        bottomNode = emptyNode
    in  IMap.fromList [(rootIdx, rootNode), (bottomIdx, bottomNode)]

initSTree :: InputText -> STree3 
initSTree input = STree3 {text3 = input, nodes3 = nodes', rootId3 = 1, bottomId3 = 0}
           where 
           nodes'  = initNodes 0 1


type BuildS = State BuildState

initBuildState :: InputText -> BuildState
initBuildState input =
    let tree = initSTree input
    in BuildState
        { buildTree = tree
        , nextId    = rootId3 tree + 1
        , activeS   = rootId3 tree
        , activeK   = 0
        }

getRootId :: BuildS NodeId
getRootId = gets (rootId3 . buildTree)

getBottomId :: BuildS NodeId
getBottomId = gets (bottomId3 . buildTree)

getActiveS :: BuildS NodeId 
getActiveS = gets activeS 

getActiveK :: BuildS Int  
getActiveK = gets activeK 

getTree :: BuildS STree3 
getTree = gets buildTree 

setActiveS :: NodeId -> BuildS ()
setActiveS sid = modify' (\st -> st {activeS = sid})

setActiveK :: Int -> BuildS ()
setActiveK k = modify' (\st -> st {activeK = k})

putTree :: STree3 -> BuildS ()
putTree t = modify' $ \st -> st { buildTree = t }

modifyTree :: (STree3 -> STree3) -> BuildS ()
modifyTree f = modify' $ \st -> st { buildTree = f (buildTree st) }

freshId :: BuildS NodeId
freshId = do
  nid <- gets nextId
  modify' (\st -> st { nextId = nid + 1 })
  pure nid

newNode :: BuildS NodeId
newNode = do
    nid <- freshId 
    modifyTree (\tree -> tree {nodes3 = IMap.insert nid emptyNode (nodes3 tree)} )
    pure nid

sTree3 :: InputText -> STree3
sTree3 input =
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
    let currentChar = B.index (text3 tree) maxIdx
    (endPoint, r) <- testAndSplit s (k, maxIdx - 1) currentChar
    go currentChar (rootId3 tree) s k endPoint r
  where
    go currentChar oldr currentS currentK endPoint r
        | endPoint = do
            tree <- getTree
            when (oldr /= rootId3 tree) $
                setSuffixLink oldr currentS
            pure (currentS, currentK)
        | otherwise = do
            _ <- addLeaf r maxIdx
            tree <- getTree
            when (oldr /= rootId3 tree) $
                setSuffixLink oldr r
            nextS <- nodeSuffixLink currentS
            (canonS, canonK) <- canonize nextS (currentK, maxIdx - 1) maxIdx
            (endPoint', r') <- testAndSplit canonS (canonK, maxIdx - 1) currentChar
            go currentChar r canonS canonK endPoint' r'

setSuffixLink :: NodeId -> NodeId -> BuildS ()
setSuffixLink n targetId =
    modifyTree $ \tree ->
        tree { nodes3 = IMap.adjust (\nd -> nd { suffixLink3 = Just targetId }) n (nodes3 tree) }

addLeaf :: NodeId -> Int -> BuildS NodeId 
addLeaf parent startIdx = do 
    tree <- getTree
    leafId <- newNode
    let edge = Edge3 { start3 = startIdx, end3 = Nothing, target3 = leafId }
        firstChar = B.index (text3 tree) startIdx
    setTransition parent firstChar edge 
    pure leafId 

nodeSuffixLink :: NodeId -> BuildS NodeId 
nodeSuffixLink nid = do 
    n <- lookupNode nid
    case suffixLink3 n of 
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
    let edgeEnd = maybe maxIdx id (end3 edge)
    pure (target3 edge, start3 edge, edgeEnd)

setTransition :: NodeId -> Word8 -> Edge3 -> BuildS ()
setTransition n t edge =
    modifyTree $ \tree ->
        tree
          { nodes3 =
              IMap.adjust
                (\nd -> nd { children3 = IMap.insert (fromIntegral t) edge (children3 nd) })
                n
                (nodes3 tree)
          }

testAndSplit :: NodeId -> (Int, Int) -> Word8 -> BuildS (Bool, NodeId)
testAndSplit s (k, p) t
    | k <= p = do
        tree <- getTree
        edge <- lookupTransition s k
        let k' = start3 edge
            splitEnd = k' + p - k
            nextChar = B.index (text3 tree) (splitEnd + 1)
        if t == nextChar
           then pure (True, s)
           else do
               r <- newNode
               let prefixEdge = Edge3 { start3 = k', end3 = Just splitEnd, target3 = r }
                   suffixEdge = Edge3 { start3 = splitEnd + 1, end3 = end3 edge, target3 = target3 edge }
               setTransition s (B.index (text3 tree) k') prefixEdge
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
    pure (IMap.member (fromIntegral t) (children3 n))

    

lookupNode :: NodeId -> BuildS Node3  
lookupNode nid = do
    nodes <- fmap nodes3 getTree
    case IMap.lookup nid nodes of 
        Just n -> pure n 
        Nothing   -> error "Node not present, wierd" 

lookupTransition :: NodeId -> Int -> BuildS Edge3
lookupTransition s k
    = do
        tree <- getTree
        if s == bottomId3 tree
           then pure Edge3 { start3 = k, end3 = Just k, target3 = rootId3 tree }
           else case IMap.lookup s (nodes3 tree) of
                  Nothing -> error "Node not present, weird"
                  Just snode ->
                      let key = fromIntegral $ B.index (text3 tree) k
                      in case IMap.lookup key (children3 snode) of
                           Nothing   -> error "Edge not present, weird"
                           Just edge -> pure edge


containsString3 :: STree3 -> InputText -> Bool
containsString3 _ patternText
    | B.null patternText = True
containsString3 tree patternText = goNode (rootId3 tree) 0
  where
    patternLen = B.length patternText
    textLen = B.length (text3 tree)

    goNode currentNode patternIdx
        | patternIdx >= patternLen = True
        | otherwise =
            case IMap.lookup currentNode (nodes3 tree) of
                Nothing -> False
                Just current ->
                    let key = fromIntegral (B.index patternText patternIdx)
                    in case IMap.lookup key (children3 current) of
                        Nothing -> False
                        Just edge -> goEdge edge patternIdx (start3 edge)

    goEdge edge patternIdx textIdx
        | patternIdx >= patternLen = True
        | textIdx > edgeStop edge = goNode (target3 edge) patternIdx
        | textIdx >= textLen = False
        | B.index patternText patternIdx == B.index (text3 tree) textIdx =
            goEdge edge (patternIdx + 1) (textIdx + 1)
        | otherwise = False

    edgeStop edge = maybe (textLen - 1) id (end3 edge)