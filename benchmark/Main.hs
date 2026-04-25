{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import           Control.DeepSeq (NFData (..), force)
import           Criterion.Main (bench, bgroup, defaultMain, env, nf)
import qualified Data.ByteString as B
import qualified SuffixTree as ST1
import qualified SuffixTree2 as ST2
import qualified SuffixTree3 as ST3
import qualified SuffixTree4 as ST4

instance NFData ST1.Edge where
    rnf (ST1.Edge start end target) = rnf start `seq` rnf end `seq` rnf target

instance NFData ST1.Node where
    rnf (ST1.Node suffixLink children) = rnf suffixLink `seq` rnf children

instance NFData ST1.STree where
    rnf (ST1.STree text nodes rootId bottomId) =
        rnf text `seq` rnf nodes `seq` rnf rootId `seq` rnf bottomId

instance NFData ST2.Edge where
    rnf (ST2.Edge start end target) = rnf start `seq` rnf end `seq` rnf target

instance NFData ST2.Node where
    rnf (ST2.Node suffixLink children) = rnf suffixLink `seq` rnf children

instance NFData ST2.STree where
    rnf (ST2.STree text nodes rootId bottomId) =
        rnf text `seq` rnf nodes `seq` rnf rootId `seq` rnf bottomId

instance NFData ST3.Edge where
    rnf (ST3.Edge start end target) = rnf start `seq` rnf end `seq` rnf target

instance NFData ST3.Node where
    rnf (ST3.Node suffixLink children) = rnf suffixLink `seq` rnf children

instance NFData ST3.STree where
    rnf (ST3.STree text nodes rootId bottomId) =
        rnf text `seq` rnf nodes `seq` rnf rootId `seq` rnf bottomId

instance NFData ST4.Edge where
    rnf (ST4.Edge start end target) = rnf start `seq` end `seq` rnf target

instance NFData ST4.Node where
    rnf (ST4.Node suffixLink children) = rnf suffixLink `seq` rnf children

instance NFData ST4.STree where
    rnf (ST4.STree text nodes rootId bottomId) =
        rnf text `seq` rnf nodes `seq` rnf rootId `seq` rnf bottomId

main :: IO ()
main =
    defaultMain
        [ env (B.readFile "whale.txt") $ \whale ->
            bgroup
                "build"
                [ bench "SuffixTree" $ nf (force . ST1.sTree) whale
                , bench "SuffixTree2" $ nf (force . ST2.sTree) whale
                , bench "SuffixTree3" $ nf (force . ST3.sTree) whale
                , bench "SuffixTree4" $ nf (force . ST4.sTree) whale
                ]
        ]
