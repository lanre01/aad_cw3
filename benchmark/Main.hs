{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.DeepSeq (NFData (..), force)
import           Criterion.Main (bench, bgroup, defaultMain, env, nf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.List (foldl')
import qualified SuffixTree5 as ST
import qualified SubstringSearch as SS
import Control.Monad (replicateM)
import System.Random (randomRIO)

instance NFData ST.Edge where
    rnf (ST.Edge s e t) = rnf s `seq` e `seq` rnf t

instance NFData ST.Node where
    rnf (ST.Node sl ch) = sl `seq` rnf ch

instance NFData ST.STree where
    rnf (ST.STree txt ns rid bid) =
        rnf txt `seq` rnf ns `seq` rnf rid `seq` rnf bid

readTestStrings :: IO [B.ByteString]
readTestStrings = do
    rawPatterns <- B.readFile "benchmark/stringTests.txt"
    pure $ filter (not . B.null) (B8.lines rawPatterns)

readWhale :: IO B.ByteString
readWhale = B.readFile "whale.txt"

totalBytes :: [B.ByteString] -> Int
totalBytes = foldl' (\acc bs -> acc + B.length bs) 0

-- takeByBytes :: Int -> [B.ByteString] -> [B.ByteString]
-- takeByBytes limit = go 0
--   where
--     go !_ [] = []
--     go !n (x:xs)
--       | n >= limit = []
--       | otherwise =
--           let n' = n + B.length x
--           in x : go n' xs

sizes :: [Int]
sizes= [2 ^ k | k <- [0 .. 20 :: Int]]

--------------------------------------------------------------------------------
-- Workloads
--------------------------------------------------------------------------------

countMatchesText :: (B.ByteString -> B.ByteString -> [Int]) -> B.ByteString -> [B.ByteString] -> Int
countMatchesText f txt =
    foldl' (\acc pat -> if null (f txt pat) then acc else acc + 1) 0

countMatchesSTree :: (ST.STree -> B.ByteString -> [Int]) -> ST.STree -> [B.ByteString] -> Int
countMatchesSTree f tree =
    foldl' (\acc pat -> if null (f tree pat) then acc else acc + 1) 0

searchAllNaive :: B.ByteString -> [B.ByteString] -> Int
searchAllNaive txt pats = countMatchesText SS.naiveSearch txt pats

searchAllKMP :: B.ByteString -> [B.ByteString] -> Int
searchAllKMP txt pats = countMatchesText SS.kmpSearch txt pats

searchAllSTree :: ST.STree -> [B.ByteString] -> Int
searchAllSTree tree pats = countMatchesSTree ST.findOccurrences tree pats

buildAndSearchSTree :: B.ByteString -> [B.ByteString] -> Int
buildAndSearchSTree txt pats =
    let tree = ST.sTree txt
    in searchAllSTree tree pats

{-
Write a function that generates two random numbers, one for the index and one for the lenght 
read the file from the index to the lenght
write back into search_test.txt
-}

type ByteString = B.ByteString

slice :: Int -> Int -> ByteString -> ByteString
slice start len bs = B.take len (B.drop start bs)

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
    let content' = filter (not . B.null) content
    B8.writeFile "stringTests.txt" (B8.unlines content')

main :: IO ()
main = do
    whale <- readWhale
    testStrings <- readTestStrings

    let batchFor :: Int -> [B.ByteString]
        batchFor n = take n testStrings

    defaultMain
      [
        -- bgroup "KMP"
        --   [ let pats = batchFor n
        --     in bench (show n) $
        --          nf (searchAllKMP whale) pats
        --   | n <- sizes
        --   ],
        bgroup "naive"
          [ let pats = batchFor n
            in bench (show n) $
                 nf (searchAllNaive whale) pats
          | n <- sizes
          ]

    

    --   bgroup "STree search"
    --       [ let pats = batchFor n
    --         in env (pure $ force (ST.sTree whale)) $ \tree ->
    --              bench (show n) $
    --                nf (`searchAllSTree` pats) tree
    --       | n <- sizes
    --       ]

    --   , bgroup "STree build+search"
    --       [ let pats = batchFor n
    --         in bench (show n) $
    --              nf (buildAndSearchSTree whale) pats
    --       | n <- sizes
    --       ]
      ]