module Main where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.IntMap as IMap
import           SuffixTree
import           SuffixTree3

main :: IO ()
main = do
  whale <- ByteString.readFile "whale.txt"
  let tree = sTree whale
      rootNode =
        case IMap.lookup (rootId tree) (nodes tree) of
          Just n -> n
          Nothing -> error "Root node missing"
      tree3 = sTree3 whale

  putStrLn "Built suffix tree for whole whale.txt"
  putStrLn $ "Input bytes: " ++ show (ByteString.length whale)
  putStrLn $ "Total nodes: " ++ show (IMap.size (nodes tree))
  putStrLn $ "Root node id: " ++ show (rootId tree)
  putStrLn $ "Bottom node id: " ++ show (bottomId tree)
  putStrLn $ "Root outgoing edges: " ++ show (IMap.size (children rootNode))

  putStrLn ""
  putStrLn "Substring checks:"
  mapM_ (printQuery whale tree)
    [ "Call me Ishmael"
    , "Loomings"
    , "whale"
    , "definitely-not-in-the-book"
    ]

  putStrLn ""
  putStrLn "Built ST-based SuffixTree3 for whole whale.txt"
  putStrLn $ "Input bytes: " ++ show (ByteString.length whale)
  putStrLn $ "Total nodes (SuffixTree3): " ++ show (IMap.size (nodes3 tree3))
  putStrLn $ "Root node id (SuffixTree3): " ++ show (rootId3 tree3)

  putStrLn ""
  putStrLn "SuffixTree3 substring checks:"
  mapM_ (printQuery3 whale tree3)
    [ "Call me Ishmael"
    , "Loomings"
    , "whale"
    , "definitely-not-in-the-book"
    ]

printQuery :: ByteString.ByteString -> STree -> String -> IO ()
printQuery source tree query = do
  let queryBytes = ByteStringChar8.pack query
      actual = containsString tree queryBytes
      expected = queryBytes `ByteString.isInfixOf` source
  putStrLn $
    query
      ++ " -> "
      ++ show actual
      ++ " (expected "
      ++ show expected
      ++ ")"

printQuery3 :: ByteString.ByteString -> STree3 -> String -> IO ()
printQuery3 source tree query = do
  let queryBytes = ByteStringChar8.pack query
      actual = containsString3 tree queryBytes
      expected = queryBytes `ByteString.isInfixOf` source
  putStrLn $
    query
      ++ " -> "
      ++ show actual
      ++ " (expected "
      ++ show expected
      ++ ")"
