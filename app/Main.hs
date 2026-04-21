module Main where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.IntMap as IMap
import           SuffixTree

main :: IO ()
main = do
  whale <- ByteString.readFile "whale.txt"
  let tree = sTree whale
      rootNode =
        case IMap.lookup (rootId tree) (nodes tree) of
          Just n -> n
          Nothing -> error "Root node missing"

  putStrLn "Built suffix tree for whale.txt"
  putStrLn $ "Input bytes: " ++ show (ByteString.length whale)
  putStrLn $ "Total nodes: " ++ show (IMap.size (nodes tree))
  putStrLn $ "Root node id: " ++ show (rootId tree)
  putStrLn $ "Bottom node id: " ++ show (bottomId tree)
  putStrLn $ "Root outgoing edges: " ++ show (IMap.size (children rootNode))

  putStrLn ""
  putStrLn "Substring checks:"
  mapM_ (printQuery tree)
    [ ("Call me Ishmael", True)
    , ("whale", True)
    , ("Moby-Dick", True)
    , ("suffix tree", False)
    , ("definitely-not-in-the-book", False)
    ]

printQuery :: STree -> (String, Bool) -> IO ()
printQuery tree (query, expected) = do
  let actual = containsString tree (ByteStringChar8.pack query)
  putStrLn $
    query
      ++ " -> "
      ++ show actual
      ++ " (expected "
      ++ show expected
      ++ ")"
