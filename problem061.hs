import Data.Ratio

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

main :: IO ()
{-
main = putStrLn $ show $ answer
    where list = [(triangle t, square s, pentagonal p, hexagonal h1, heptagonal h2, octagonal o) |
                      t <- [45..140],
                      (length $ show $ triangle t) == 4,
                      s <- [(a_square ((get_lower $ triangle t) * 100))..(a_square ((get_lower $ triangle t) * 100 + 100))],
                      (length $ show $ square s) == 4,
                      p <- [(a_pentagonal ((get_lower $ square s) * 100))..(a_pentagonal ((get_lower $ square s) * 100 + 100))],
                      (length $ show $ pentagonal p) == 4,
                      h1 <- [(a_hexagonal ((get_lower $ pentagonal p) * 100))..(a_hexagonal ((get_lower $ pentagonal p) * 100 + 100))],
                      (length $ show $ hexagonal h1) == 4,
                      h2 <- [(a_heptagonal ((get_lower $ hexagonal h1) * 100))..(a_heptagonal ((get_lower $ hexagonal h1) * 100 + 100))],
                      (length $ show $ heptagonal h2) == 4,
                      o <- [(a_octagonal ((get_lower $ heptagonal h2) * 100))..(a_octagonal ((get_lower $ heptagonal h2) * 100 + 100))],
                      (length $ show $ octagonal o) == 4,
                      (get_lower $ triangle t) == (get_upper $ square s) &&
                      (get_lower $ square s) == (get_upper $ pentagonal p) &&
                      (get_lower $ pentagonal p) == (get_upper $ hexagonal h1) &&
                      (get_lower $ hexagonal h1) == (get_upper $ heptagonal h2) &&
                      (get_lower $ heptagonal h2) == (get_upper $ octagonal o) &&
                      (get_lower $ octagonal o) == (get_upper $ triangle t)]
          answer = list
-}
{-
main = do putStrLn $ show $ takeWhile (\x -> (snd x) < 10000) $ dropWhile (\x -> (snd x) < 1000) $ map (\x -> (x, triangle x)) [1..]
          putStrLn $ show $ takeWhile (\x -> (snd x) < 10000) $ dropWhile (\x -> (snd x) < 1000) $ map (\x -> (x, square x)) [1..]
          putStrLn $ show $ takeWhile (\x -> (snd x) < 10000) $ dropWhile (\x -> (snd x) < 1000) $ map (\x -> (x, pentagonal x)) [1..]
          putStrLn $ show $ takeWhile (\x -> (snd x) < 10000) $ dropWhile (\x -> (snd x) < 1000) $ map (\x -> (x, hexagonal x)) [1..]
          putStrLn $ show $ takeWhile (\x -> (snd x) < 10000) $ dropWhile (\x -> (snd x) < 1000) $ map (\x -> (x, heptagonal x)) [1..]
          putStrLn $ show $ takeWhile (\x -> (snd x) < 10000) $ dropWhile (\x -> (snd x) < 1000) $ map (\x -> (x, octagonal x)) [1..]
-}
{-
main = do putStrLn $ show $ (a_triangle 1000, a_triangle 10000)
          putStrLn $ show $ (a_square 1000, a_square 10000)
          putStrLn $ show $ (a_pentagonal 1000, a_pentagonal 10000)
          putStrLn $ show $ (a_hexagonal 1000, a_hexagonal 10000)
          putStrLn $ show $ (a_heptagonal 1000, a_heptagonal 10000)
          putStrLn $ show $ (a_octagonal 1000, a_octagonal 10000)
-}
main = do putStrLn $ show $ take 10 $ filter is_triangle [1..]
          putStrLn $ show $ take 10 $ filter is_square [1..]
          putStrLn $ show $ take 10 $ filter is_pentagonal [1..]
          putStrLn $ show $ take 10 $ filter is_hexagonal [1..]
          putStrLn $ show $ take 10 $ filter is_heptagonal [1..]
          putStrLn $ show $ take 10 $ filter is_octagonal [1..]
