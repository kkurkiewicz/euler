{- |
Module      :  Euler.Problem040
Description :  Problem 40 - Champernowne's constant
Copyright   :  (c) Kamil Kurkiewicz
License     :  NONE

Maintainer  :  Kamil Kurkiewicz <k-kurkiewicz@outlook.com>
Stability   :  provisional
Portability :  portable

Fast implementation of a function calculating single digits of the
base-10 Champernowne decimal
-}
module Euler.Problem040 where


import qualified CLI
import           Control.Monad


------------------------------------------------------------


-- |UNUSED Total number of @n@-digit positive integers
a :: Integral a => a -> a
a n
    | n < 1     = error "Euler.Problem040.a: n too small"
    | otherwise = 9 * 10^(n-1)


-- |Total number of characters needed to write down all numbers of
-- @n@ digits
b :: Integral a => a -> a
b n
    | n < 1     = error "Euler.Problem040.b: n too small"
    | otherwise = 9*n * 10^(n-1)


-- |Implements \(d_n\)
d :: Int -> Int
d n
    | n < 1 || n > 10^18 = error "Euler.Problem040.d: parameter out of bounds"
    | otherwise =
              let
                  -- (1)
                  scan   = takeWhile (< n) $ scanl1 (+) [b m | m <- [1..]]
                  (s, l) =
                          case scan of
                              []    -> (0, 0)
                              (_:_) -> (last scan, length scan)
                  -- (2)
                  (q, r) = divMod (n-s) (l+1)
                  s'     = 10^l - 1
              in
                  -- (3)
                  if r > 0
                      then digits (q + s' + 1) 10 !! (l - r + 1)
                      else mod (q+s') 10


-- |Returns the digits of a positive integer as a list of ints
-- 
-- TODO Check for duplicated code
-- 
digits :: Int -> Int -> [Int]
digits number base
    | base < 1  = error "Euler.Problem040.digits: bases smaller than one not supported"
    | otherwise = aux number base
        where
          aux 0 _ = []
          aux n 1 = take n $ repeat 1
          aux n b = r : aux q b
            where
              (q, r) = quotRem n b


-- |Solves the HackerRank modification of the problem
hackerrank :: IO ()
hackerrank = do
    -- Number of test cases
    t <- getLine
    
    -- 
    replicateM_ (read t) $ do
        ns <- getLine
        print $ product [d n | n <- conv ns]
  where
    conv = map read . words


-- |Prints the answer to the original Project Euler question
-- 
-- NOTE Should only print one line
-- 
projecteuler :: IO ()
projecteuler = do
    let ans = product [d n | n <- [10^k | k <- [0 .. 6]]]
    print ans


-- |Point of entry
main :: IO ()
main = CLI.cliMain projecteuler (Just hackerrank) Nothing "Euler.Problem040"

