module SuffixTree where 

import qualified Data.ByteString as ByteString 
import qualified Data.IntMap as IMap 
import           Data.Word (Word8)
import           Data.List (foldl')

type InputText   = ByteString.ByteString
type NodeId = Int


data Edge = Edge
  { start  :: !Int
  , end    :: !(Maybe Int)
  , target :: !NodeId
  }

data Node = Node
  { suffixLink :: !(Maybe NodeId)
  , children   :: !(IMap.IntMap Edge)
  }

data STree = STree
  { text   :: !InputText
  , nodes  :: !(IMap.IntMap Node)
  , rootId   :: !NodeId
  , bottomId :: !NodeId
  }

initSTree :: InputText -> STree 
initSTree input = STree {text = input, nodes = nodes', rootId = rootIdx, bottomId = rootIdx + 1}
           where 
           rootIdx = ByteString.length input 
           nodes'  = initNodes rootIdx 
emptyNode :: Node 
emptyNode = Node {suffixLink = Nothing, children = IMap.empty}

node :: NodeId -> Node 
node suffixId = Node {suffixLink = Just suffixId, children = IMap.empty}

initNodes :: NodeId -> IMap.IntMap Node
initNodes rootIdx = 
    let rootNode = node (rootIdx + 1)
        bottomNode = emptyNode
    in  IMap.fromList [(rootIdx, rootNode), (rootIdx + 1, bottomNode)]

sTree :: InputText -> STree 
sTree input = 
    let initialSTree = initSTree input
        s = rootId initialSTree
        lastIdx = ByteString.length input - 1
        (finalStTree, _) = foldl' someFunc (initialSTree, (s, 0)) [0..lastIdx] 
    in finalStTree

-- will do two things 
-- update 
-- canonize
someFunc :: (STree, (NodeId, Int)) -> Int -> (STree, (NodeId, Int))
someFunc (tree, (s, k)) i =
    let (tree', (s', k')) = update tree (s, k) i
        activePoint = canonize tree' s' (k', i) i
    in (tree', activePoint)

update :: STree -> (NodeId, Int) -> Int -> (STree, (NodeId, Int))
update tree (s, k) maxIdx = finish finalTree finalOldr finalS finalK
  where
    currentChar = ByteString.index (text tree) maxIdx
    (endPoint, r, tree') = testAndSplit tree s (k, maxIdx - 1) currentChar maxIdx
    (finalTree, finalOldr, finalS, finalK) = loop tree' (rootId tree') s k endPoint r

    loop currentTree oldr currentS currentK endPoint' r'
        | endPoint' =
            (currentTree, oldr, currentS, currentK)
        | otherwise =
            let (treeWithLeaf, _) = addLeaf currentTree r' maxIdx
                treeWithSuffix =
                    if oldr /= rootId treeWithLeaf
                        then setSuffixLink treeWithLeaf oldr r'
                        else treeWithLeaf
                oldr' = r'
                nextS = nodeSuffixLink treeWithSuffix currentS
                (canonS, canonK) = canonize treeWithSuffix nextS (currentK, maxIdx - 1) maxIdx
                (endPoint'', r'', nextTree) =
                    testAndSplit treeWithSuffix canonS (canonK, maxIdx - 1) currentChar maxIdx
            in loop nextTree oldr' canonS canonK endPoint'' r''

    finish currentTree oldr currentS currentK
        | oldr /= rootId currentTree = (setSuffixLink currentTree oldr currentS, (currentS, currentK))
        | otherwise = (currentTree, (currentS, currentK))


canonize :: STree -> NodeId -> (Int, Int) -> Int -> (NodeId, Int)
canonize tree s (k, i) maxIdx
   | i < k  = (s, k)
   | otherwise = 
      let (s', k', p') = findTkTransition tree s k maxIdx
      in canonize' tree (s, k, i) (s', k', p') maxIdx

canonize' :: STree -> (NodeId, Int, Int) -> (NodeId, Int, Int) -> Int -> (NodeId, Int)
canonize' tree (s, k, p) (s', k', p') maxIdx
    | p' - k' <= p - k = 
        let k'' = k + p' - k' + 1
        in if k'' <= p then let (s'', k''', p'') = findTkTransition tree s' k'' maxIdx
                            in canonize' tree (s', k'', p) (s'', k''', p'') maxIdx
           else (s', k'')
    | otherwise = (s, k)                         

findTkTransition :: STree -> NodeId -> Int -> Int -> (NodeId, Int, Int)
findTkTransition tree s k maxIdx =
    let edge = lookupTransition tree s k maxIdx
        edgeEnd =
            case end edge of
                Just e  -> e
                Nothing -> maxIdx
    in (target edge, start edge, edgeEnd)

testAndSplit :: STree -> NodeId -> (Int, Int) -> Word8 -> Int -> (Bool, NodeId, STree)
testAndSplit tree s (k, p) t maxIdx
    | k <= p =
        let edge = lookupTransition tree s k maxIdx
            k' = start edge
            splitEnd = k' + p - k
            nextChar = ByteString.index (text tree) (splitEnd + 1)
        in if t == nextChar
              then (True, s, tree)
              else
                  let (treeWithSplit, r) = newNode tree
                      prefixEdge = Edge {start = k', end = Just splitEnd, target = r}
                      suffixEdge = Edge {start = splitEnd + 1, end = end edge, target = target edge}
                      tree1 = setTransition treeWithSplit s (ByteString.index (text tree) k') prefixEdge
                      tree2 = setTransition tree1 r nextChar suffixEdge
                  in (False, r, tree2)
    | hasTransition tree s t = (True, s, tree)
    | otherwise = (False, s, tree)

lookupTransition :: STree -> NodeId -> Int -> Int -> Edge
lookupTransition tree s k _
    | s == bottomId tree = Edge {start = k, end = Just k, target = rootId tree}
lookupTransition tree s k _ =
    case IMap.lookup s (nodes tree) of
        Nothing -> error "Node not present, weird"
        Just snode ->
            case IMap.lookup key (children snode) of
                Nothing   -> error "Edge not present, weird"
                Just edge -> edge
  where
    key = fromIntegral $ ByteString.index (text tree) k

hasTransition :: STree -> NodeId -> Word8 -> Bool
hasTransition tree s _
    | s == bottomId tree = True
hasTransition tree s t =
    case IMap.lookup s (nodes tree) of
        Nothing    -> False
        Just snode -> IMap.member (fromIntegral t) (children snode)

newNode :: STree -> (STree, NodeId)
newNode tree =
    let nextId =
            case IMap.lookupMax (nodes tree) of
                Nothing     -> 0
                Just (n, _) -> n + 1
        tree' = tree {nodes = IMap.insert nextId emptyNode (nodes tree)}
    in (tree', nextId)

setSuffixLink :: STree -> NodeId -> NodeId -> STree
setSuffixLink tree n targetId =
    tree {nodes = IMap.adjust (\nd -> nd {suffixLink = Just targetId}) n (nodes tree)}

nodeSuffixLink :: STree -> NodeId -> NodeId
nodeSuffixLink tree n =
    case IMap.lookup n (nodes tree) >>= suffixLink of
        Just link -> link
        Nothing   -> error "Missing suffix link"

setTransition :: STree -> NodeId -> Word8 -> Edge -> STree
setTransition tree n t edge =
    tree
        { nodes =
            IMap.adjust
                (\nd -> nd {children = IMap.insert (fromIntegral t) edge (children nd)})
                n
                (nodes tree)
        }

addLeaf :: STree -> NodeId -> Int -> (STree, NodeId)
addLeaf tree parent startIdx =
    let (tree', leafId) = newNode tree
        edge = Edge {start = startIdx, end = Nothing, target = leafId}
        firstChar = ByteString.index (text tree) startIdx
    in (setTransition tree' parent firstChar edge, leafId)

containsString :: STree -> InputText -> Bool
containsString _ patternText
    | ByteString.null patternText = True
containsString tree patternText = goNode (rootId tree) 0
  where
    patternLen = ByteString.length patternText
    textLen = ByteString.length (text tree)

    goNode currentNode patternIdx
        | patternIdx >= patternLen = True
        | otherwise =
            case IMap.lookup currentNode (nodes tree) of
                Nothing -> False
                Just current ->
                    let key = fromIntegral $ ByteString.index patternText patternIdx
                    in case IMap.lookup key (children current) of
                        Nothing -> False
                        Just edge -> goEdge edge patternIdx (start edge)

    goEdge edge patternIdx textIdx
        | patternIdx >= patternLen = True
        | textIdx > edgeStop edge = goNode (target edge) patternIdx
        | textIdx >= textLen = False
        | ByteString.index patternText patternIdx == ByteString.index (text tree) textIdx =
            goEdge edge (patternIdx + 1) (textIdx + 1)
        | otherwise = False

    edgeStop edge = maybe (textLen - 1) id (end edge)
