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

funcs = [(triangle, a_triangle, is_triangle),
         (square, a_square, is_square),
         (pentagonal, a_pentagonal, is_pentagonal),
         (hexagonal, a_hexagonal, is_hexagonal),
         (heptagonal, a_heptagonal, is_heptagonal),
         (octagonal, a_octagonal, is_octagonal)]

process []       = []
process (fs:fss) = if length list > 0 then list : process fss else process fss
    where [(f1, a_f1, is_f1), (f2, a_f2, is_f2), (f3, a_f3, is_f3), (f4, a_f4, is_f4), (f5, a_f5, is_f5), (f6, a_f6, is_f6)] = fs
          list = [(f1 r1, f2 r2, f3 r3, f4 r4, f5 r5, f6 r6) |
                  r1 <- [(a_f1 1000)..(a_f1 10000)],
                  (f1 r1 >= 1000) && (f1 r1 < 10000),
                  r2 <- [(a_f2 ((get_lower $ f1 r1) * 100))..(a_f2 ((get_lower $ f1 r1) * 100 + 100))],
                  (f2 r2 >= 1000) && (f2 r2 < 10000),
                  r3 <- [(a_f3 ((get_lower $ f2 r2) * 100))..(a_f3 ((get_lower $ f2 r2) * 100 + 100))],
                  (f3 r3 >= 1000) && (f3 r3 < 10000),
                  r4 <- [(a_f4 ((get_lower $ f3 r3) * 100))..(a_f4 ((get_lower $ f3 r3) * 100 + 100))],
                  (f4 r4 >= 1000) && (f4 r4 < 10000),
                  r5 <- [(a_f5 ((get_lower $ f4 r4) * 100))..(a_f5 ((get_lower $ f4 r4) * 100 + 100))],
                  (f5 r5 >= 1000) && (f5 r5 < 10000),
                  r6 <- [(a_f6 ((get_lower $ f5 r5) * 100))..(a_f6 ((get_lower $ f5 r5) * 100 + 100))],
                  (f6 r6 >= 1000) && (f6 r6 < 10000),
                  (get_lower $ f1 r1) == (get_upper $ f2 r2),
                  (get_lower $ f2 r2) == (get_upper $ f3 r3),
                  (get_lower $ f3 r3) == (get_upper $ f4 r4),
                  (get_lower $ f4 r4) == (get_upper $ f5 r5),
                  (get_lower $ f5 r5) == (get_upper $ f6 r6),
                  (get_lower $ f6 r6) == (get_upper $ f1 r1)]

main :: IO ()
main = putStrLn $ show $ nub $ map (\(r1, r2, r3, r4, r5, r6) -> r1 + r2 + r3 + r4 + r5 + r6) $ concat $ process $ permutations funcs
