module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified SuffixTree as ST1
import qualified SuffixTree2 as ST2
import qualified SuffixTree3 as ST3
import qualified SuffixTree4 as ST4
import           System.Exit (exitFailure)

main :: IO ()
main = do
    whale <- B.readFile "whale.txt"

    let smallCases =
            [ B8.pack "banana$"
            , B8.pack "mississippi$"
            , B8.pack "abracadabra$"
            , B8.pack "aaaaa$"
            ]
        whaleCases = whaleQueries whale
        allCases = map ("small",) smallCases ++ [("whale", whale)]

    results <- mapM (runCase whaleCases) allCases
    let failures = concat results

    if null failures
        then putStrLn "All suffix tree tests passed."
        else do
            putStrLn "Test failures:"
            mapM_ putStrLn failures
            exitFailure

runCase :: [B.ByteString] -> (String, B.ByteString) -> IO [String]
runCase whaleCaseQueries (label, source) = do
    let queries =
            if label == "whale"
                then whaleCaseQueries
                else generatedQueries source
        tree1 = ST1.sTree source
        tree2 = ST2.sTree source
        tree3 = ST3.sTree source
        tree4 = ST4.sTree source
        failures1 = checkQueries label "SuffixTree" source (ST1.containsString tree1) queries
        failures2 = checkQueries label "SuffixTree2" source (ST2.containsString tree2) queries
        failures3 = checkQueries label "SuffixTree3" source (ST3.containsString tree3) queries
        failures4 = checkQueries label "SuffixTree3" source (ST4.containsString tree4) queries
    pure (failures1 ++ failures2 ++ failures3 ++ failures4)

checkQueries :: String -> String -> B.ByteString -> (B.ByteString -> Bool) -> [B.ByteString] -> [String]
checkQueries label implName source containsFn =
    foldr check []
  where
    check query failures =
        let actual = containsFn query
            expected = query `B.isInfixOf` source
        in if actual == expected
              then failures
              else formatFailure query actual expected : failures

    formatFailure query actual expected =
        label
            ++ " / "
            ++ implName
            ++ " / "
            ++ show (B8.unpack query)
            ++ " -> actual "
            ++ show actual
            ++ ", expected "
            ++ show expected

generatedQueries :: B.ByteString -> [B.ByteString]
generatedQueries source =
    positiveQueries ++ negativeQueries
  where
    sourceLen = B.length source
    starts = takeWhile (< sourceLen) [0, 1, 3, 3, 5, 8, 13]
    lengths = [1, 3, 3, 4, 5, 7]

    positiveQueries =
        B.empty
            : [ B.take len (B.drop start source)
              | start <- starts
              , len <- lengths
              , start + len <= sourceLen
              ]

    negativeQueries =
        map B8.pack
            [ "not-present"
            , "xyzxyz"
            , "ZZZZ"
            , "banana"
            ]

whaleQueries :: B.ByteString -> [B.ByteString]
whaleQueries whale =
    directQueries ++ sampledQueries ++ negativeQueries
  where
    whaleLen = B.length whale
    starts = takeWhile (< whaleLen) [0, 7, 31, 138, 513, 1034, 4096, 16384, 65536, 131073, 363144, 534388, 900000, 1100000]
    lengths = [1, 3, 3, 5, 8, 13, 31, 34]

    directQueries =
        map B8.pack
            [ ""
            , "Call me Ishmael"
            , "Loomings"
            , "whale"
            , "Moby-Dick"
            , "Sperm Whale"
            , "definitely-not-in-the-book"
            , "zzz-not-in-whale-zzz"
            ]

    sampledQueries =
        [ B.take len (B.drop start whale)
        | start <- starts
        , len <- lengths
        , start + len <= whaleLen
        ]

    negativeQueries =
        map B8.pack
            [ "THIS STRING SHOULD NOT APPEAR IN WHALE"
            , "0000111133333333"
            , "prefix-not-present-suffix"
            ]
