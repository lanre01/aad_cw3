module Main where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.IntMap as IMap
import qualified SuffixTree as ST1
import qualified SuffixTree2 as ST2
import qualified SuffixTree3 as ST3
import qualified SuffixTree4 as ST4

main :: IO ()
main = do
  whale <- ByteString.readFile "whale.txt"
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

  putStrLn "Built SuffixTree for whole whale.txt"
  putStrLn $ "Input bytes: " ++ show (ByteString.length whale)
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
  putStrLn $ "Input bytes: " ++ show (ByteString.length whale)
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
  putStrLn $ "Input bytes: " ++ show (ByteString.length whale)
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
  putStrLn $ "Input bytes: " ++ show (ByteString.length whale)
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

printQuery1 :: ByteString.ByteString -> ST1.STree -> String -> IO ()
printQuery1 source tree query = do
  let queryBytes = ByteStringChar8.pack query
      actual = ST1.containsString tree queryBytes
      expected = queryBytes `ByteString.isInfixOf` source
  putStrLn $
    query
      ++ " -> "
      ++ show actual
      ++ " (expected "
      ++ show expected
      ++ ")"

printQuery2 :: ByteString.ByteString -> ST2.STree -> String -> IO ()
printQuery2 source tree query = do
  let queryBytes = ByteStringChar8.pack query
      actual = ST2.containsString tree queryBytes
      expected = queryBytes `ByteString.isInfixOf` source
  putStrLn $
    query
      ++ " -> "
      ++ show actual
      ++ " (expected "
      ++ show expected
      ++ ")"

printQuery3 :: ByteString.ByteString -> ST3.STree -> String -> IO ()
printQuery3 source tree query = do
  let queryBytes = ByteStringChar8.pack query
      actual = ST3.containsString tree queryBytes
      expected = queryBytes `ByteString.isInfixOf` source
  putStrLn $
    query
      ++ " -> "
      ++ show actual
      ++ " (expected "
      ++ show expected
      ++ ")"

printQuery4 :: ByteString.ByteString -> ST4.STree -> String -> IO ()
printQuery4 source tree query = do
  let queryBytes = ByteStringChar8.pack query
      actual = ST4.containsString tree queryBytes
      expected = queryBytes `ByteString.isInfixOf` source
  putStrLn $
    query
      ++ " -> "
      ++ show actual
      ++ " (expected "
      ++ show expected
      ++ ")"
