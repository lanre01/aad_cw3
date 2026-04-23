module MyLib (someFunc) where

-- import qualified Data.ByteString as B
-- import qualified Data.IntMap.Strict as IMap
-- import           Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
-- import           Data.Word (Word8)
-- import           Control.Monad (forM_, when)
-- import           Control.Monad.ST (ST, runST)
someFunc :: IO ()
someFunc = putStrLn "someFunc"



-- type InputText = B.ByteString
-- type NodeId = Int

-- data Edge2 = Edge2
--   { start2  :: !Int
--   , end2    :: !Int
--   , target2 :: !NodeId
--   }

-- data Node2 = Node2
--   { terminal2 :: !Bool
--   , children2 :: !(IMap.IntMap Edge2)
--   }

-- data STree2 = STree2
--   { text2   :: !InputText
--   , nodes2  :: !(IMap.IntMap Node2)
--   , rootId2 :: !NodeId
--   }

-- data MNode s = MNode
--   { mTerminal :: !(STRef s Bool)
--   , mChildren :: !(STRef s (IMap.IntMap Edge2))
--   }

-- data Builder s = Builder
--   { builderText   :: !InputText
--   , builderNodes  :: !(STRef s (IMap.IntMap (MNode s)))
--   , nextNodeIdRef :: !(STRef s Int)
--   , rootNodeId    :: !NodeId
--   }

-- sTree2 :: InputText -> STree2
-- sTree2 input = runST $ do
--     rootNode <- newMNode False
--     nodesRef <- newSTRef (IMap.singleton 0 rootNode)
--     nextIdRef <- newSTRef 1
--     let builder = Builder
--             { builderText = input
--             , builderNodes = nodesRef
--             , nextNodeIdRef = nextIdRef
--             , rootNodeId = 0
--             }
--         textLen = B.length input
--     forM_ [0 .. textLen - 1] $ \suffixStart ->
--         insertSuffix builder suffixStart
--     freezeBuilder builder

-- containsString2 :: STree2 -> InputText -> Bool
-- containsString2 _ patternText
--     | B.null patternText = True
-- containsString2 tree patternText = walkNode (rootId2 tree) 0
--   where
--     patternLen = B.length patternText

--     walkNode nodeId patternIdx
--         | patternIdx >= patternLen = True
--         | otherwise =
--             case IMap.lookup nodeId (nodes2 tree) of
--                 Nothing -> False
--                 Just node ->
--                     let key = fromIntegral (B.index patternText patternIdx)
--                     in case IMap.lookup key (children2 node) of
--                         Nothing -> False
--                         Just edge -> walkEdge edge patternIdx (start2 edge)

--     walkEdge edge patternIdx textIdx
--         | patternIdx >= patternLen = True
--         | textIdx > end2 edge = walkNode (target2 edge) patternIdx
--         | B.index patternText patternIdx == B.index (text2 tree) textIdx =
--             walkEdge edge (patternIdx + 1) (textIdx + 1)
--         | otherwise = False

-- newMNode :: Bool -> ST s (MNode s)
-- newMNode isTerminal = do
--     terminalRef <- newSTRef isTerminal
--     childrenRef <- newSTRef IMap.empty
--     pure MNode {mTerminal = terminalRef, mChildren = childrenRef}

-- freezeBuilder :: Builder s -> ST s STree2
-- freezeBuilder builder = do
--     mutableNodes <- readSTRef (builderNodes builder)
--     frozenNodes <- traverse freezeNode mutableNodes
--     pure STree2
--         { text2 = builderText builder
--         , nodes2 = frozenNodes
--         , rootId2 = rootNodeId builder
--         }

-- freezeNode :: MNode s -> ST s Node2
-- freezeNode mNode = do
--     isTerminal <- readSTRef (mTerminal mNode)
--     outgoing <- readSTRef (mChildren mNode)
--     pure Node2 {terminal2 = isTerminal, children2 = outgoing}

-- insertSuffix :: Builder s -> Int -> ST s ()
-- insertSuffix builder suffixStart = insertAt builder (rootNodeId builder) suffixStart

-- insertAt :: Builder s -> NodeId -> Int -> ST s ()
-- insertAt builder nodeId suffixIdx = do
--     let input = builderText builder
--         textLen = B.length input
--     if suffixIdx >= textLen
--         then markTerminal builder nodeId
--         else do
--             currentNode <- getNode builder nodeId
--             outgoing <- readSTRef (mChildren currentNode)
--             let firstByte = B.index input suffixIdx
--                 key = fromIntegral firstByte
--             case IMap.lookup key outgoing of
--                 Nothing -> do
--                     leafId <- newNodeId builder True
--                     let leafEdge = Edge2 {start2 = suffixIdx, end2 = textLen - 1, target2 = leafId}
--                     setChild builder nodeId firstByte leafEdge
--                 Just edge -> descendOrSplit builder nodeId suffixIdx firstByte edge

-- descendOrSplit :: Builder s -> NodeId -> Int -> Word8 -> Edge2 -> ST s ()
-- descendOrSplit builder parentId suffixIdx firstByte edge = do
--     let input = builderText builder
--         textLen = B.length input
--         matched = commonPrefixLength input suffixIdx (start2 edge) (end2 edge)
--         edgeLen = end2 edge - start2 edge + 1
--         suffixEnd = suffixIdx + matched
--     if matched == edgeLen
--         then insertAt builder (target2 edge) suffixEnd
--         else do
--             splitId <- newNodeId builder (suffixEnd >= textLen)
--             let prefixEdge = Edge2
--                     { start2 = start2 edge
--                     , end2 = start2 edge + matched - 1
--                     , target2 = splitId
--                     }
--                 oldTailStart = start2 edge + matched
--                 oldTailEdge = Edge2
--                     { start2 = oldTailStart
--                     , end2 = end2 edge
--                     , target2 = target2 edge
--                     }
--             setChild builder parentId firstByte prefixEdge
--             setChild builder splitId (B.index input oldTailStart) oldTailEdge
--             when (suffixEnd < textLen) $ do
--                 leafId <- newNodeId builder True
--                 let newTailEdge = Edge2
--                         { start2 = suffixEnd
--                         , end2 = textLen - 1
--                         , target2 = leafId
--                         }
--                 setChild builder splitId (B.index input suffixEnd) newTailEdge

-- commonPrefixLength :: InputText -> Int -> Int -> Int -> Int
-- commonPrefixLength input suffixIdx edgeStart edgeEnd = go suffixIdx edgeStart 0
--   where
--     textLen = B.length input

--     go i j matched
--         | i >= textLen = matched
--         | j > edgeEnd = matched
--         | B.index input i == B.index input j = go (i + 1) (j + 1) (matched + 1)
--         | otherwise = matched

-- newNodeId :: Builder s -> Bool -> ST s NodeId
-- newNodeId builder isTerminal = do
--     nextId <- readSTRef (nextNodeIdRef builder)
--     newNode <- newMNode isTerminal
--     modifySTRef' (builderNodes builder) (IMap.insert nextId newNode)
--     writeSTRef (nextNodeIdRef builder) (nextId + 1)
--     pure nextId

-- getNode :: Builder s -> NodeId -> ST s (MNode s)
-- getNode builder nodeId = do
--     currentNodes <- readSTRef (builderNodes builder)
--     case IMap.lookup nodeId currentNodes of
--         Just node -> pure node
--         Nothing -> error "SuffixTree2: missing node"

-- markTerminal :: Builder s -> NodeId -> ST s ()
-- markTerminal builder nodeId = do
--     node <- getNode builder nodeId
--     writeSTRef (mTerminal node) True

-- setChild :: Builder s -> NodeId -> Word8 -> Edge2 -> ST s ()
-- setChild builder nodeId firstByte edge = do
--     node <- getNode builder nodeId
--     modifySTRef' (mChildren node) (IMap.insert (fromIntegral firstByte) edge)
