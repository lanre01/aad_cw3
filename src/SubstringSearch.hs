{-# LANGUAGE ScopedTypeVariables #-}

-- | Baseline substring-search algorithms over strict 'ByteString' values,
-- for benchmarking against the suffix-tree approach.
--
-- Both functions return a list of all starting positions at which the
-- pattern occurs in the text.
--
-- * 'naiveSearch': slides a window over the text and compares character by
--   character, running in O(n m) worst-case time.
--
-- * 'kmpSearch': builds the KMP failure function in O(m) and scans the
--   text in a single O(n) pass, giving O(n + m) total per query.
module SubstringSearch (
    naiveSearch,
    kmpSearch
) where

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import           Control.Monad.ST

-- --------------------------------------------------------------------------
-- Naive O(nm) search
-- --------------------------------------------------------------------------

-- | Find all occurrences of @pat@ in @txt@ by brute-force comparison.
--
-- For each candidate position @i@ in @[0 .. n - m]@ the pattern is compared
-- byte-by-byte against @txt[i ..]@.  The worst case is O(n m).
naiveSearch :: B.ByteString -> B.ByteString -> [Int]
naiveSearch txt pat
    | m == 0    = []
    | m > n     = []
    | otherwise = go 0
  where
    n = B.length txt
    m = B.length pat

    go :: Int -> [Int]
    go i
        | i > n - m  = []
        | matchAt i 0 = i : go (i + 1)
        | otherwise   = go (i + 1)

    matchAt :: Int -> Int -> Bool
    matchAt _ j
        | j >= m = True
    matchAt i j
        | B.index txt (i + j) /= B.index pat j = False
        | otherwise = matchAt i (j + 1)

-- --------------------------------------------------------------------------
-- KMP O(n + m) search
-- --------------------------------------------------------------------------

-- | Find all occurrences of @pat@ in @txt@ using the Knuth-Morris-Pratt
-- algorithm.
--
-- The failure function is built in O(m) time using a mutable unboxed vector,
-- and the text is scanned in a single O(n) pass.  Total cost per call is
-- O(n + m).
kmpSearch :: B.ByteString -> B.ByteString -> [Int]
kmpSearch txt pat
    | m == 0    = []
    | m > n     = []
    | otherwise = search 0 0
  where
    n = B.length txt
    m = B.length pat

    -- Failure function: failure ! j is the length of the longest proper
    -- prefix of pat[0..j] that is also a suffix of pat[0..j].
    failure :: VU.Vector Int
    failure = runST $ do
        f <- MVU.replicate m 0
        let build i len
                | i >= m = pure ()
                | B.index pat i == B.index pat len = do
                    let len' = len + 1
                    MVU.write f i len'
                    build (i + 1) len'
                | len > 0 = do
                    prev <- MVU.read f (len - 1)
                    build i prev
                | otherwise = do
                    MVU.write f i 0
                    build (i + 1) 0
        build 1 0
        VU.unsafeFreeze f

    -- Scan the text, emitting match positions.
    search :: Int -> Int -> [Int]
    search i j
        | i >= n    = []
        | B.index txt i == B.index pat j =
            let j' = j + 1
            in if j' == m
               then (i - m + 1) : search (i + 1) (failure VU.! (j' - 1))
               else search (i + 1) j'
        | j > 0     = search i (failure VU.! (j - 1))
        | otherwise = search (i + 1) 0
