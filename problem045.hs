import Data.List
import Debug.Trace

{-
triangle :: (Integral a, Show a) => a -> a
triangle 1 = 1
triangle n = n + triangle (n - 1)

pentagonal :: (Integral a, Show a) => a -> a
pentagonal 1 = 1
pentagonal n = 3 * (n - 1) + 1 + pentagonal (n - 1)

hexagonal :: (Integral a, Show a) => a -> a
hexagonal 1 = 1
hexagonal n = 4 * (n - 1) + 1 + hexagonal (n - 1)
-}

triangle :: (Integral a, Show a) => a -> a
triangle n = n * (n + 1) `div` 2

pentagonal :: (Integral a, Show a) => a -> a
pentagonal n = n * (3 * n - 1) `div` 2

hexagonal :: (Integral a, Show a) => a -> a
hexagonal n = n * (2 * n - 1)

process :: (Integral a, Show a) => a -> a -> a -> [(a, a, a)] -> [(a, a, a)]
process l m n xs | t == p && p == h = (l, m, n) : xs
                 | t == mn          = process (l + 1) m n xs
                 | p == mn          = process l (m + 1) n xs
                 | h == mn          = process l m (n + 1) xs
    where min3 l m n = min (min l m) n
          t = triangle l
          p = pentagonal m
          h = hexagonal n
          mn = if n `mod` 100 == 0 then trace ("l = " ++ (show l) ++ ", m = " ++ (show m) ++ ", n = " ++ (show n) ++ ", t = " ++ (show t) ++ ", p = " ++ (show p) ++ ", h = " ++ (show h)) min3 t p h else min3 t p h

main :: IO ()
main = putStr $ show $ map (\(l, m, n) -> ((l, triangle l), (m, pentagonal m), (n, hexagonal n)))$ process 286 165 143 []
