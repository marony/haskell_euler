{-
http://projecteuler.net/thread=26

14 Feb 2006 02:43 pm 
kelan
Haskell  

A Haskell solution, taking advantage of the fact that the period of a number n's reciprocal is the smallest k such that n divides 10^k - 1.
-}
import Data.List

nums = [ n | n <- [3,5..], n `mod` 5 /= 0 ]

period n =
    head $ [ p | p <- [1..], (10^p - 1) `mod` n == 0 ]

answer =
    fst $
    maximumBy (\(_,a) (_,b) -> compare a b) $
    map (\n -> (n,period n)) $
    takeWhile (<1000) nums

main = putStr $ show answer
