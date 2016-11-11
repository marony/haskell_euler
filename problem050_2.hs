{-
http://projecteuler.net/thread=50&page=9#119144

18 Jun 2013 04:09 pm 
hakenstock
Haskell

Took about 0.02s.
-}

import Data.List (tails)

primes = 3 : filter prime [5,7..]

prime n = all ((/= 0) . mod n) $ takeWhile small primes
  where small x = x^2 <= n

mChain = reverse . takeWhile ((< 10^6) . snd) . zip [1..] . scanl1 (+)

pChain = dropWhile (notPrime . snd) . mChain
  where notPrime n = even n || not (prime n)

search x (xs:xss)
  | fst x > l = x
  | otherwise = search (max x y) xss
  where (l,_) = head (mChain xs)
        (y:_) = pChain xs

euler50 = search (0,0) $ tails (2:primes)

main = putStr $ show euler50
