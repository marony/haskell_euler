import Data.Ratio

import Debug.Trace


-- x = -1 * (sqrt(8 * y + 1) + 1) / 2
-- x = (sqrt(8 * y + 1) - 1) / 2
-- triangle 45 = 1035
-- triangle 140 = 9870
triangle :: Int -> Int
triangle x   = x * (x + 1) `div` 2
-- x = -1 * sqrt(y)
-- x = sqrt(y)
square :: Int -> Int
square x     = x * x
is_square :: Int -> Bool
is_square x = (square (floor y) == x) || square (floor y') == x
    where y  = -1 * sqrt(fromIntegral x :: Double)
          y' = sqrt(fromIntegral x :: Double)
-- x = -1 * (sqrt(24 * y + 1) - 1) / 6
-- x = (sqrt(24 * y + 1) + 1) / 6
pentagonal :: Int -> Int
pentagonal x = x * (3 * x - 1) `div` 2
is_pentagonal :: Int -> Bool
is_pentagonal x = (pentagonal (floor y) == x) || (pentagonal (floor y') == x)
    where y  = -1 * (sqrt(24 * (fromIntegral x :: Double) + 1) - 1) / 6
          y' = (sqrt(24 * (fromIntegral x :: Double) + 1) + 1) / 6
-- x = -1 * (sqrt(8 * y + 1) - 1) / 4
-- x = (sqrt(8 * y + 1) + 1) / 4
hexagonal :: Int -> Int
hexagonal x  = x * (2 * x - 1)
is_hexagonal :: Int -> Bool
is_hexagonal x = (hexagonal (floor y) == x) || (hexagonal (floor y') == x)
    where y  = -1 * (sqrt(8 * (fromIntegral x :: Double) + 1) - 1) / 4
          y' = (sqrt(8 * (fromIntegral x :: Double) + 1) + 1) / 4
-- x = -1 * (sqrt(40 * y + 9) - 3) / 10
-- x = (sqrt(40 * y + 9) + 3) / 10
heptagonal :: Int -> Int
heptagonal x = x * (5 * x - 3) `div` 2
is_heptagonal :: Int -> Bool
is_heptagonal x = (heptagonal (floor y) == x) || (heptagonal (floor y') == x)
    where y  = -1 * (sqrt(40 * (fromIntegral x :: Double) + 9) - 3) / 10
          y' = (sqrt(40 * (fromIntegral x :: Double) + 9) + 3) / 10
-- x = -1 * (sqrt(3 * y + 1) - 1) / 3
-- x = (sqrt(3 * y + 1) + 1) / 3
octagonal :: Int -> Int
octagonal x  = x * (3 * x - 2)
is_octagonal :: Int -> Bool
is_octagonal x = (octagonal (floor y) == x) || (octagonal (floor y') == x)
    where y  = -1 * (sqrt(3 * (fromIntegral x :: Double) + 1) - 1) / 3
          y' = (sqrt(3 * (fromIntegral x :: Double) + 1) + 1) / 3

get_lower x = x `mod` 100
get_upper x = x `div` 100

process_triangle :: [(Int, Int)] -> [[(Int, Int)]]
process_triangle []     = []
process_triangle (t:ts) = (map (\s -> [t] ++ s) ss) ++ (process_triangle ts)
    where ss = (process_square t) $ filter (is_square . snd) $ [(n, (get_lower $ snd t) * 100 + n) | n <- [10..99]]

process_square :: (Int, Int) -> [(Int, Int)] -> [[(Int, Int)]]
process_square t []     = []
process_square t (s:ss) = (map (\p -> [s] ++ p) ps) ++ (process_square t ss)
    where ps = (process_pentagonal t) $ filter (is_pentagonal . snd) $ [(n, (get_lower $ snd s) * 100 + n) | n <- [10..99]]

process_pentagonal :: (Int, Int) -> [(Int, Int)] -> [[(Int, Int)]]
process_pentagonal t []     = []
process_pentagonal t (p:ps) = (map (\h -> [p] ++ h) hs) ++ (process_pentagonal t ps)
    where hs = (process_hexagonal t) $ filter (is_hexagonal . snd) $ [(n, (get_lower $ snd p) * 100 + n) | n <- [10..99]]

process_hexagonal :: (Int, Int) -> [(Int, Int)] -> [[(Int, Int)]]
process_hexagonal t []     = []
process_hexagonal t (h:hs) = (map (\h2 -> [h] ++ h2) h2s) ++ (process_hexagonal t hs)
    where h2s = (process_heptagonal t) $ filter (is_heptagonal . snd) $ [(n, (get_lower $ snd h) * 100 + n) | n <- [10..99]]

process_heptagonal :: (Int, Int) -> [(Int, Int)] -> [[(Int, Int)]]
process_heptagonal t []     = []
process_heptagonal t (h:hs) = (map (\o -> [h] ++ [o]) os) ++ (process_heptagonal t hs)
    where os = filter (\o -> (get_upper $ snd t) == (get_lower $ snd o)) $ filter (is_octagonal . snd) $ [(n, (get_lower $ snd h) * 100 + n) | n <- [10..99]]

sum' :: [(Int, Int)] -> Int
sum' xs = foldl (\x y -> x + (snd y)) 0 xs

main :: IO ()
main = putStrLn $ show $ (sum' answer, answer)
    where answer = concat $ process_triangle [(n, triangle n) | n <- [45..140]]
-- (28280,[(130,8515),(21,1521),(47,2147),(53,4753),(59,5359),(85,5985)])

{-
main = do putStrLn $ show $ map triangle [1..10]
          putStrLn $ show $ map square [1..10]
          putStrLn $ show $ map pentagonal [1..10]
          putStrLn $ show $ map hexagonal [1..10]
          putStrLn $ show $ map heptagonal [1..10]
          putStrLn $ show $ map octagonal [1..10]
-}
