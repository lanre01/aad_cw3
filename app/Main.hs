module Main where

import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as C8
import qualified Data.IntMap as IMap
import qualified SuffixTree1 as ST1
import qualified SuffixTree2 as ST2
import qualified SuffixTree3 as ST3
import qualified SuffixTree4 as ST4
import qualified SuffixTree as ST5
import Control.Monad (replicateM)
import System.Random (randomRIO)
import Data.Word (Word8)

{-
Write a function that generates two random numbers, one for the index and one for the lenght 
read the file from the index to the lenght
write back into search_test.txt
-}

type ByteString = B.ByteString

slice :: Int -> Int -> ByteString -> ByteString
slice start len bs = B.take len (B.drop start bs)

space :: Word8
space = 32

trimFront :: B.ByteString -> B.ByteString
trimFront = B.dropWhile (== space)

trimBack :: B.ByteString -> B.ByteString
trimBack = B.dropWhileEnd (== space)

trim :: B.ByteString -> B.ByteString
trim = trimFront . trimBack


generateAllSamples :: ByteString -> IO [ByteString]
generateAllSamples whale
  | maxLen == 0 = pure []
  | otherwise = do
      starts <- replicateM sampleCount (randomRIO (0, maxLen - 1))
      lens   <- replicateM sampleCount (randomRIO (0, 20))
      let pairs = zipWith adjust starts lens
      pure [slice s l whale | (s, l) <- pairs]
  where
    sampleCount = 1_200_000
    maxLen = B.length whale

    adjust :: Int -> Int -> (Int, Int)
    adjust start len =
      let safeLen = min len (maxLen - start)
      in (start, safeLen)

writeToFile :: IO ()
writeToFile = do
    whale   <- B.readFile "whale.txt"
    content <- generateAllSamples whale
    let content' = map trim $ filter (not . B.null) content
    C8.writeFile "stringTests2.txt" (C8.unlines content')


main :: IO ()
main = do
  writeToFile
  putStrLn "done writing test strings to file"
  whale <- B.readFile "whale.txt"
  let tree1 = ST1.sTree whale
      rootNode1 =
        case IMap.lookup (ST1.rootId tree1) (ST1.nodes tree1) of
          Just n -> n
          Nothing -> error "Root node missing"
      tree2 = ST2.sTree whale
      rootNode2 =
        case IMap.lookup (ST2.rootId tree2) (ST2.nodes tree2) of
          Just n -> n
          Nothing -> error "Root node missing"
      tree3 = ST3.sTree whale
      rootNode3 =
        case IMap.lookup (ST3.rootId tree3) (ST3.nodes tree3) of
          Just n -> n
          Nothing -> error "Root node missing"
      tree4 = ST4.sTree whale
      rootNode4 = 
        case IMap.lookup (ST4.rootId tree4) (ST4.nodes tree4) of
          Just n -> n
          Nothing -> error "Root node missing"
      tree5 = ST5.sTree whale
      rootNode5 = ST5.nodes tree5 V.! (ST5.rootId tree5)

  putStrLn "Built SuffixTree for whole whale.txt"
  putStrLn $ "Input bytes: " ++ show (B.length whale)
  putStrLn $ "Total nodes: " ++ show (IMap.size (ST1.nodes tree1))
  putStrLn $ "Root node id: " ++ show (ST1.rootId tree1)
  putStrLn $ "Bottom node id: " ++ show (ST1.bottomId tree1)
  putStrLn $ "Root outgoing edges: " ++ show (IMap.size (ST1.children rootNode1))

  putStrLn ""
  putStrLn "Substring checks:"
  mapM_ (printQuery1 whale tree1)
    [ "Call me Ishmael"
    , "Loomings"
    , "whale"
    , "definitely-not-in-the-book"
    ]

  putStrLn ""
  putStrLn "Built SuffixTree2 for whole whale.txt"
  putStrLn $ "Input bytes: " ++ show (B.length whale)
  putStrLn $ "Total nodes (SuffixTree2): " ++ show (IMap.size (ST2.nodes tree2))
  putStrLn $ "Root node id (SuffixTree2): " ++ show (ST2.rootId tree2)
  putStrLn $ "Bottom node id (SuffixTree2): " ++ show (ST2.bottomId tree2)
  putStrLn $ "Root outgoing edges (SuffixTree2): " ++ show (IMap.size (ST2.children rootNode2))

  putStrLn ""
  putStrLn "SuffixTree2 substring checks:"
  mapM_ (printQuery2 whale tree2)
    [ "Call me Ishmael"
    , "Loomings"
    , "whale"
    , "definitely-not-in-the-book"
    ]

  putStrLn ""
  putStrLn "Built SuffixTree3 for whole whale.txt"
  putStrLn $ "Input bytes: " ++ show (B.length whale)
  putStrLn $ "Total nodes (SuffixTree3): " ++ show (IMap.size (ST3.nodes tree3))
  putStrLn $ "Root node id (SuffixTree3): " ++ show (ST3.rootId tree3)
  putStrLn $ "Bottom node id (SuffixTree3): " ++ show (ST3.bottomId tree3)
  putStrLn $ "Root outgoing edges (SuffixTree3): " ++ show (IMap.size (ST3.children rootNode3))

  putStrLn ""
  putStrLn "SuffixTree3 substring checks:"
  mapM_ (printQuery3 whale tree3)
    [ "Call me Ishmael"
    , "Loomings"
    , "whale"
    , "definitely-not-in-the-book"
    ]

  putStrLn ""
  putStrLn "Built SuffixTree4 for whole whale.txt"
  putStrLn $ "Input bytes: " ++ show (B.length whale)
  putStrLn $ "Total nodes (SuffixTree4): " ++ show (IMap.size (ST4.nodes tree4))
  putStrLn $ "Root node id (SuffixTree4): " ++ show (ST4.rootId tree4)
  putStrLn $ "Bottom node id (SuffixTree4): " ++ show (ST4.bottomId tree4)
  putStrLn $ "Root outgoing edges (SuffixTree4): " ++ show (IMap.size (ST4.children rootNode4))

  putStrLn ""
  putStrLn "SuffixTree4 substring checks:"
  mapM_ (printQuery4 whale tree4)
    [ "Call me Ishmael"
    , "Loomings"
    , "whale"
    , "definitely-not-in-the-book"
    ]
  
  putStrLn ""
  putStrLn "Built SuffixTree5 for whole whale.txt"
  putStrLn $ "Input bytes: " ++ show (B.length whale)
  putStrLn $ "Total nodes (SuffixTree5): " ++ show (V.length (ST5.nodes tree5))
  putStrLn $ "Root node id (SuffixTree5): " ++ show (ST5.rootId tree5)
  putStrLn $ "Bottom node id (SuffixTree5): " ++ show (ST5.bottomId tree5)
  putStrLn $ "Root outgoing edges (SuffixTree5): " ++ show (IMap.size (ST5.children rootNode5))

  putStrLn ""
  putStrLn "SuffixTree5 substring checks:"
  mapM_ (printQuery4 whale tree4)
    [ "Call me Ishmael"
    , "Loomings"
    , "whale"
    , "definitely-not-in-the-book"
    ]

printQuery1 :: ByteString -> ST1.STree -> String -> IO ()
printQuery1 source tree query = do
  let queryBytes = C8.pack query
      actual = ST1.containsString tree queryBytes
      expected = queryBytes `B.isInfixOf` source
  putStrLn $
    query
      ++ " -> "
      ++ show actual
      ++ " (expected "
      ++ show expected
      ++ ")"

printQuery2 :: ByteString -> ST2.STree -> String -> IO ()
printQuery2 source tree query = do
  let queryBytes = C8.pack query
      actual = ST2.containsString tree queryBytes
      expected = queryBytes `B.isInfixOf` source
  putStrLn $
    query
      ++ " -> "
      ++ show actual
      ++ " (expected "
      ++ show expected
      ++ ")"

printQuery3 :: ByteString -> ST3.STree -> String -> IO ()
printQuery3 source tree query = do
  let queryBytes = C8.pack query
      actual = ST3.containsString tree queryBytes
      expected = queryBytes `B.isInfixOf` source
  putStrLn $
    query
      ++ " -> "
      ++ show actual
      ++ " (expected "
      ++ show expected
      ++ ")"

printQuery4 :: ByteString -> ST4.STree -> String -> IO ()
printQuery4 source tree query = do
  let queryBytes = C8.pack query
      actual = ST4.containsString tree queryBytes
      expected = queryBytes `B.isInfixOf` source
  putStrLn $
    query
      ++ " -> "
      ++ show actual
      ++ " (expected "
      ++ show expected
      ++ ")"
