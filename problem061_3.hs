import Data.List

import Debug.Trace

{-
    Triangle = x * (x + 1) / 2

x = -1 * (sqrt(8 * y + 1) + 1) / 2
x = (sqrt(8 * y + 1) - 1) / 2

triangle 45 = 1035
triangle 140 = 9870
-}
triangle :: Int -> Int
triangle x   = x * (x + 1) `div` 2

a_triangle :: Int -> Int
a_triangle x = floor $ (sqrt(fromIntegral (8 * x + 1)) - 1) / 2

is_triangle :: Int -> Bool
is_triangle x = (realToFrac $ floor y) == y
    where y = (sqrt(8 * (fromIntegral x :: Double) + 1) - 1) / 2

{-
    Square = x ^ 2

x = -1 * sqrt(y)
x = sqrt(y)

square 32 = 1024
square 99 = 9801
-}
square :: Int -> Int
square x = x * x

a_square :: Int -> Int
a_square x = floor . sqrt . fromIntegral $ x

is_square :: Int -> Bool
is_square x = (realToFrac $ floor y) == y
    where y = sqrt(fromIntegral x :: Double)

{-
    Pentagonal = x * (3 * x - 1) / 2

x = -1 * (sqrt(24 * y + 1) - 1) / 6
x = (sqrt(24 * y + 1) + 1) / 6

pentagonal 26 = 1001
pentagonal 81 = 9801
-}
pentagonal :: Int -> Int
pentagonal x = x * (3 * x - 1) `div` 2

a_pentagonal :: Int -> Int
a_pentagonal x = floor $ (sqrt(fromIntegral (24 * x + 1)) + 1) / 6

is_pentagonal :: Int -> Bool
is_pentagonal x = (realToFrac $ floor y) == y
    where y = (sqrt(24 * (fromIntegral x :: Double) + 1) + 1) / 6

{-
    Hexagonal = x * (2 * x - 1)

x = -1 * (sqrt(8 * y + 1) - 1) / 4
x = (sqrt(8 * y + 1) + 1) / 4

hexagonal 23 = 1035
hexagonal 70 = 9730
-}
hexagonal :: Int -> Int
hexagonal x  = x * (2 * x - 1)

a_hexagonal :: Int -> Int
a_hexagonal x = floor $ (sqrt(fromIntegral (8 * x + 1)) + 1) / 4

is_hexagonal :: Int -> Bool
is_hexagonal x = (realToFrac $ floor y) == y
    where y = (sqrt(8 * (fromIntegral x :: Double) + 1) + 1) / 4

{-
    Heptagonal = x * (5 * x - 3) / 2

x = -1 * (sqrt(40 * y + 9) - 3) / 10
x = (sqrt(40 * y + 9) + 3) / 10

heptagonal 21 = 1071
heptagonal 63 = 9828

-}
heptagonal :: Int -> Int
heptagonal x = x * (5 * x - 3) `div` 2

a_heptagonal :: Int -> Int
a_heptagonal x = floor $ (sqrt(fromIntegral (40 * x + 9)) + 3) / 10

is_heptagonal :: Int -> Bool
is_heptagonal x = (realToFrac $ floor y) == y
    where y = (sqrt(40 * (fromIntegral x :: Double) + 9) + 3) / 10

{-
    Octagonal = x * (3 * x - 2)

x = -1 * (sqrt(3 * y + 1) - 1) / 3
x = (sqrt(3 * y + 1) + 1) / 3

octagonal 19 = 1045
octagonal 58 = 9976

-}
octagonal :: Int -> Int
octagonal x  = x * (3 * x - 2)

a_octagonal :: Int -> Int
a_octagonal x = floor $ (sqrt(fromIntegral (3 * x + 1)) + 1) / 3

is_octagonal :: Int -> Bool
is_octagonal x = (realToFrac $ floor y) == y
    where y = (sqrt(3 * (fromIntegral x :: Double) + 1) + 1) / 3


get_lower x = x `mod` 100
get_upper x = x `div` 100

process_triangle :: Int -> Int -> [Int]
process_triangle 0     0        = map triangle range
    where range = [45..140]
process_triangle first previous = map triangle range
    where lower = (get_lower $ previous) * 100
          range = [(a_triangle lower)..(a_triangle (lower + 100))]

process_square :: Int -> Int -> [Int]
process_square 0     0        = map square range
    where range = [32..99]
process_square first previous = map square range
    where lower = (get_lower $ previous) * 100
          range = [(a_square lower)..(a_square (lower + 100))]

funcs = [process_triangle, process_square]

process_funcss :: [[(Int -> Int -> [Int])]] -> [[Int]]
process_funcss []       = []
process_funcss (fs:fss) = a ++ process_funcss fss
    where a = process_funcs fs 0 0 0

process_funcs :: [(Int -> Int -> [Int])] -> Int -> Int -> Int -> [[Int]]
process_funcs [] _ _ _                = []
process_funcs (f:fs) n first previous = process_result rs fs n first previous : process_funcs fs n first previous
    where rs = f first previous

process_result :: [Int] -> [(Int -> Int -> [Int])] -> Int -> Int -> Int -> [Int]
process_result [] _ _ _ _                 = []
process_result (r:rs) fs n first previous = process_result' r fs n first previous

process_result' :: Int -> [(Int -> Int -> [Int])] -> Int -> Int -> Int -> [Int]
process_result' _ [] _ _ _                = []
process_result' r (f:fs) 0 0 0            = r : (process_result rs fs 1 r r)
    where rs = f r r
process_result' r (f:fs) n first previous = r : process_result rs fs (n + 1) first r
    where rs = f first previous

main :: IO ()
main = putStrLn $ show $ list
    where list = process_funcss $ permutations funcs
