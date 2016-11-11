import Data.List
import Debug.Trace

is_prime :: Integral a => a -> Bool
is_prime n | n < 2     = False
           | otherwise = is_prime' n 2
               where is_prime' :: Integral a => a -> a -> Bool
                     is_prime' n d
                         | d * d > n      = True
                         | n `mod` d == 0 = False
                         | otherwise      = is_prime' n $ d + 1

factors :: Integral a => a -> [a]
factors n = filter (\x -> factors' n x) [2..(n `div` 2)]
    where factors' :: Integral a => a -> a -> Bool
          factors' n a | a <= 1    = False
                       | otherwise = if ((n `mod` a) == 0 && is_prime a) then True else False

count = 4

process :: (Integral a, Show a) => [a] -> [(a, [a])]
process xs = process' list []
    where list = filter (\x -> (length $ snd x) == count) $ zip xs $ map factors xs
          process' :: (Integral a, Show a) => [(a, [a])] -> [(a, [a])] -> [(a, [a])]
          process' [] ys         = ys
          process' (x:xs) []     = process' xs [x]
          process' (x:xs) (y:ys) = if (fst x) == (fst y) + 1
                                   then if length (x:y:ys) >= count
                                        then (x:y:ys)
                                        else process' xs (x:y:ys)
                                   else process' xs [x]

main :: IO ()
main = putStr $ show $ process list
    where list = [2..]
