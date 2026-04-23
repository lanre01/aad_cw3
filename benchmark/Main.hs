{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import           Control.DeepSeq (NFData (..), force)
import           Criterion.Main (bench, bgroup, defaultMain, env, nf)
import qualified Data.ByteString as B
import           SuffixTree (Edge (..), Node (..), STree (..), sTree)
import           SuffixTree2 (Edge2 (..), Node2 (..), STree2 (..), sTree2)

instance NFData Edge where
    rnf (Edge start end target) = rnf start `seq` rnf end `seq` rnf target

instance NFData Node where
    rnf (Node suffixLink children) = rnf suffixLink `seq` rnf children

instance NFData STree where
    rnf (STree text nodes rootId bottomId) =
        rnf text `seq` rnf nodes `seq` rnf rootId `seq` rnf bottomId

instance NFData Edge2 where
    rnf (Edge2 start end target) = rnf start `seq` rnf end `seq` rnf target

instance NFData Node2 where
    rnf (Node2 suffixLink children) = rnf suffixLink `seq` rnf children

instance NFData STree2 where
    rnf (STree2 text nodes rootId bottomId) =
        rnf text `seq` rnf nodes `seq` rnf rootId `seq` rnf bottomId

main :: IO ()
main =
    defaultMain
        [ env (B.readFile "whale.txt") $ \whale ->
            bgroup
                "build"
                [ bench "SuffixTree" $ nf (force . sTree) whale
                , bench "SuffixTree2" $ nf (force . sTree2) whale
                ]
        ]
