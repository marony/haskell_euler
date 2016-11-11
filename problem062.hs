import Data.List
import Debug.Trace

max_count = 5

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

count_digit :: Int -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
count_digit n = (count (== '0') s, count (== '1') s, count (== '2') s,
                 count (== '3') s, count (== '4') s, count (== '5') s,
                 count (== '6') s, count (== '7') s, count (== '8') s,
                 count (== '9') s)
    where s = show n

process :: [Int] -> [(Int, (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int))] -> [(Int, (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int))]
process (n:ns) rs = if c /= max_count - 1
                    then process ns ((t, r) : rs)
                    else (t, r) : filter equal rs
    where equal (x, y) = y == r
          t = n ^ 3
          r = count_digit t
          c = count equal rs

main :: IO ()
main = putStrLn $ show $ fst $ minimumBy (\(n1, r1) (n2, r2) -> compare n1 n2) $ process [1..] []
