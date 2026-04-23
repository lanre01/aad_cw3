module SuffixTree2 (
    STree2 (..),
    Edge2 (..),
    Node2 (..),
    containsString2,
    sTree2
) where

import           Control.Monad.ST (ST, runST)
import qualified Data.ByteString as B
import qualified Data.IntMap.Strict as IMap
import           Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import           Data.Word (Word8)

type InputText = B.ByteString
type NodeId = Int

data Edge2 = Edge2
  { start2  :: !Int
  , end2    :: !(Maybe Int)
  , target2 :: !NodeId
  } deriving Show

data Node2 = Node2
  { suffixLink2 :: !(Maybe NodeId)
  , children2   :: !(IMap.IntMap Edge2)
  } deriving Show

data STree2 = STree2
  { text2     :: !InputText
  , nodes2    :: !(IMap.IntMap Node2)
  , rootId2   :: !NodeId
  , bottomId2 :: !NodeId
  } deriving Show

-- Mutable nodes used only during construction
data MNode2 s = MNode2
  { suffixLinkRef2 :: !(STRef s (Maybe NodeId))
  , childrenRef2   :: !(STRef s (IMap.IntMap Edge2))
  }

data MTree2 s = MTree2
  { mText2     :: !InputText
  , mNodes2    :: !(STRef s (IMap.IntMap (MNode2 s)))
  , mRootId2   :: !NodeId
  , mBottomId2 :: !NodeId
  }

data BuildState s = BuildState
  { bsTree2    :: !(MTree2 s)
  , bsNextId2  :: !NodeId
  , bsActiveS2 :: !NodeId
  , bsActiveK2 :: !Int
  }

type Builder s a = BuildState s -> ST s (a, BuildState s)

sTree2 :: InputText -> STree2
sTree2 input = runST $ do
    st0 <- initBuildState2 input
    let lastIdx = B.length input - 1

        go :: Int -> BuildState s -> ST s (BuildState s)
        go i st
          | i > lastIdx = pure st
          | otherwise = do
              (_, st') <- extendAtIndex2 i st
              go (i + 1) st'

    stF <- go 0 st0
    freezeTree2 (bsTree2 stF)

extendAtIndex2 :: Int -> Builder s ()
extendAtIndex2 i st = do
    let s = bsActiveS2 st
        k = bsActiveK2 st

    ((s', k'), st1) <- update2 s k i st
    ((s'', k''), st2) <- canonize2 s' (k', i) i st1

    let st3 = st2 { bsActiveS2 = s'', bsActiveK2 = k'' }
    pure ((), st3)

initBuildState2 :: InputText -> ST s (BuildState s)
initBuildState2 input = do
    nodesRef <- newSTRef IMap.empty

    rootSuffix <- newSTRef (Just 1)
    rootChildren <- newSTRef IMap.empty
    bottomSuffix <- newSTRef Nothing
    bottomChildren <- newSTRef IMap.empty

    let rootNode   = MNode2 rootSuffix rootChildren
        bottomNode = MNode2 bottomSuffix bottomChildren

    writeSTRef nodesRef $ IMap.fromList
        [ (0, rootNode)
        , (1, bottomNode)
        ]

    let mt = MTree2
              { mText2 = input
              , mNodes2 = nodesRef
              , mRootId2 = 0
              , mBottomId2 = 1
              }

    pure BuildState
        { bsTree2 = mt
        , bsNextId2 = 2
        , bsActiveS2 = 0
        , bsActiveK2 = 0
        }

freezeTree2 :: MTree2 s -> ST s STree2
freezeTree2 mt = do
    m <- readSTRef (mNodes2 mt)
    frozen <- traverse freezeNode2 m
    pure STree2
        { text2 = mText2 mt
        , nodes2 = frozen
        , rootId2 = mRootId2 mt
        , bottomId2 = mBottomId2 mt
        }

freezeNode2 :: MNode2 s -> ST s Node2
freezeNode2 mn = do
    sl <- readSTRef (suffixLinkRef2 mn)
    ch <- readSTRef (childrenRef2 mn)
    pure Node2
        { suffixLink2 = sl
        , children2 = ch
        }

lookupNode2 :: MTree2 s -> NodeId -> ST s (MNode2 s)
lookupNode2 mt n = do
    m <- readSTRef (mNodes2 mt)
    case IMap.lookup n m of
        Just node -> pure node
        Nothing   -> error "SuffixTree2: node not present"

newNode2 :: Builder s NodeId
newNode2 st = do
    let nid = bsNextId2 st
        mt  = bsTree2 st

    slRef <- newSTRef Nothing
    chRef <- newSTRef IMap.empty
    let node = MNode2 slRef chRef

    modifySTRef' (mNodes2 mt) (IMap.insert nid node)

    let st' = st { bsNextId2 = nid + 1 }
    pure (nid, st')

setSuffixLink2 :: NodeId -> NodeId -> Builder s ()
setSuffixLink2 n targetId st = do
    let mt = bsTree2 st
    node <- lookupNode2 mt n
    writeSTRef (suffixLinkRef2 node) (Just targetId)
    pure ((), st)

nodeSuffixLink2 :: NodeId -> Builder s NodeId
nodeSuffixLink2 n st = do
    let mt = bsTree2 st
    node <- lookupNode2 mt n
    msl <- readSTRef (suffixLinkRef2 node)
    case msl of
        Just link -> pure (link, st)
        Nothing   -> error "SuffixTree2: missing suffix link"

setTransition2 :: NodeId -> Word8 -> Edge2 -> Builder s ()
setTransition2 n t edge st = do
    let mt = bsTree2 st
    node <- lookupNode2 mt n
    modifySTRef' (childrenRef2 node) (IMap.insert (fromIntegral t) edge)
    pure ((), st)

lookupTransition2 :: NodeId -> Int -> Int -> Builder s Edge2
lookupTransition2 s k _ st
    | s == mBottomId2 mt =
        pure (Edge2 { start2 = k, end2 = Just k, target2 = mRootId2 mt }, st)
    | otherwise = do
        node <- lookupNode2 mt s
        children <- readSTRef (childrenRef2 node)
        let key = fromIntegral (B.index (mText2 mt) k)
        case IMap.lookup key children of
            Just edge -> pure (edge, st)
            Nothing   -> error "SuffixTree2: edge not present"
  where
    mt = bsTree2 st

hasTransition2 :: NodeId -> Word8 -> Builder s Bool
hasTransition2 s _ st
    | s == mBottomId2 mt = pure (True, st)
  where
    mt = bsTree2 st
hasTransition2 s t st = do
    let mt = bsTree2 st
    node <- lookupNode2 mt s
    children <- readSTRef (childrenRef2 node)
    pure (IMap.member (fromIntegral t) children, st)

addLeaf2 :: NodeId -> Int -> Builder s NodeId
addLeaf2 parent startIdx st = do
    let mt = bsTree2 st
    (leafId, st1) <- newNode2 st
    let edge = Edge2 { start2 = startIdx, end2 = Nothing, target2 = leafId }
        firstChar = B.index (mText2 mt) startIdx
    ((), st2) <- setTransition2 parent firstChar edge st1
    pure (leafId, st2)

findTkTransition2 :: NodeId -> Int -> Int -> Builder s (NodeId, Int, Int)
findTkTransition2 s k maxIdx st = do
    (edge, st1) <- lookupTransition2 s k maxIdx st
    let edgeEnd = maybe maxIdx id (end2 edge)
    pure ((target2 edge, start2 edge, edgeEnd), st1)

canonize2 :: NodeId -> (Int, Int) -> Int -> Builder s (NodeId, Int)
canonize2 s (k, i) maxIdx st
    | i < k = pure ((s, k), st)
    | otherwise = do
        ((s', k', p'), st1) <- findTkTransition2 s k maxIdx st
        canonizeLoop2 (s, k, i) (s', k', p') maxIdx st1

canonizeLoop2 :: (NodeId, Int, Int) -> (NodeId, Int, Int) -> Int -> Builder s (NodeId, Int)
canonizeLoop2 (s, k, p) (s', k', p') maxIdx st
    | p' - k' <= p - k = do
        let k'' = k + p' - k' + 1
        if k'' <= p
            then do
                ((s'', k''', p''), st1) <- findTkTransition2 s' k'' maxIdx st
                canonizeLoop2 (s', k'', p) (s'', k''', p'') maxIdx st1
            else pure ((s', k''), st)
    | otherwise = pure ((s, k), st)

testAndSplit2 :: NodeId -> (Int, Int) -> Word8 -> Int -> Builder s (Bool, NodeId)
testAndSplit2 s (k, p) t maxIdx st
    | k <= p = do
        let mt = bsTree2 st
        (edge, st1) <- lookupTransition2 s k maxIdx st
        let k' = start2 edge
            splitEnd = k' + p - k
            nextChar = B.index (mText2 mt) (splitEnd + 1)
        if t == nextChar
            then pure ((True, s), st1)
            else do
                (r, st2) <- newNode2 st1
                let prefixEdge = Edge2 { start2 = k', end2 = Just splitEnd, target2 = r }
                    suffixEdge = Edge2 { start2 = splitEnd + 1, end2 = end2 edge, target2 = target2 edge }
                    firstChar  = B.index (mText2 mt) k'
                ((), st3) <- setTransition2 s firstChar prefixEdge st2
                ((), st4) <- setTransition2 r nextChar suffixEdge st3
                pure ((False, r), st4)
    | otherwise = do
        (exists, st1) <- hasTransition2 s t st
        pure ((if exists then True else False, s), st1)

update2 :: NodeId -> Int -> Int -> Builder s (NodeId, Int)
update2 s k maxIdx st = do
    let mt = bsTree2 st
        currentChar = B.index (mText2 mt) maxIdx

    ((endPoint, r), st1) <- testAndSplit2 s (k, maxIdx - 1) currentChar maxIdx st
    loop currentChar (mRootId2 mt) s k endPoint r st1
  where
    loop :: Word8 -> NodeId -> NodeId -> Int -> Bool -> NodeId -> Builder s (NodeId, Int)
    loop currentChar oldr currentS currentK endPoint r st'
        | endPoint = do
            let root = mRootId2 (bsTree2 st')
            if oldr /= root
                then do
                    ((), st'') <- setSuffixLink2 oldr currentS st'
                    pure ((currentS, currentK), st'')
                else pure ((currentS, currentK), st')
        | otherwise = do
            (_, st1) <- addLeaf2 r maxIdx st'

            let root = mRootId2 (bsTree2 st1)
            st2 <-
                if oldr /= root
                    then do
                        ((), stTmp) <- setSuffixLink2 oldr r st1
                        pure stTmp
                    else pure st1

            (nextS, st3) <- nodeSuffixLink2 currentS st2
            ((canonS, canonK), st4) <- canonize2 nextS (currentK, maxIdx - 1) maxIdx st3
            ((endPoint', r'), st5) <- testAndSplit2 canonS (canonK, maxIdx - 1) currentChar maxIdx st4
            loop currentChar r canonS canonK endPoint' r' st5

containsString2 :: STree2 -> InputText -> Bool
containsString2 _ patternText
    | B.null patternText = True
containsString2 tree patternText = goNode (rootId2 tree) 0
  where
    patternLen = B.length patternText
    textLen = B.length (text2 tree)

    goNode currentNode patternIdx
        | patternIdx >= patternLen = True
        | otherwise =
            case IMap.lookup currentNode (nodes2 tree) of
                Nothing -> False
                Just current ->
                    let key = fromIntegral (B.index patternText patternIdx)
                    in case IMap.lookup key (children2 current) of
                        Nothing -> False
                        Just edge -> goEdge edge patternIdx (start2 edge)

    goEdge edge patternIdx textIdx
        | patternIdx >= patternLen = True
        | textIdx > edgeStop edge = goNode (target2 edge) patternIdx
        | textIdx >= textLen = False
        | B.index patternText patternIdx == B.index (text2 tree) textIdx =
            goEdge edge (patternIdx + 1) (textIdx + 1)
        | otherwise = False

    edgeStop edge = maybe (textLen - 1) id (end2 edge)