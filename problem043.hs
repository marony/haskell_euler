import Data.List

is_pandigital :: (Integral a, Show a) => a -> Bool
is_pandigital n = is_pandigital' (show n)
    where pandigital = "0123456789"
          is_pandigital' :: String -> Bool
          is_pandigital' n = (sort n) == pandigital

main :: IO ()
main = putStr $ show $ sum
    [d1 * (10 ^ 9) + d2 * (10 ^ 8) + d3 * (10 ^ 7) +
     d4 * (10 ^ 6) + d5 * (10 ^ 5) + d6 * (10 ^ 4) +
     d7 * (10 ^ 3) + d8 |
        d1 <- [1..9],
        d2 <- [0..9], d2 /= d1,
        d3 <- [0..9], d3 /= d2 && d3 /= d1,
        d4 <- [0,2..9], d4 /= d3 && d4 /= d2 && d4 /= d1,
        d5 <- [0..9], d5 /= d4 && d5 /= d3 && d5 /= d2 && d5 /= d1,
        d6 <- [0,5], d6 /= d5 && d6 /= d4 && d6 /= d3 && d6 /= d2 && d6 /= d1,
        d7 <- [0..9], d7 /= d6 && d7 /= d5 && d7 /= d4 && d7 /= d3 && d7 /= d2 && d7 /= d1,
        d8 <- [102,119..999],
        is_pandigital (d1 * (10 ^ 9) + d2 * (10 ^ 8) + d3 * (10 ^ 7) +
                      d4 * (10 ^ 6) + d5 * (10 ^ 5) + d6 * (10 ^ 4) +
                      d7 * (10 ^ 3) + d8),
        (d3 * (10 ^ 2) + d4 * 10 + d5) `mod` 3 == 0,
        (d5 * (10 ^ 2) + d6 * 10 + d7) `mod` 7 == 0,
        (d6 * (10 ^ 2) + d7 * 10 + (d8 `div` 100)) `mod` 11 == 0,
        (d7 * (10 ^ 2) + (d8 `div` 10)) `mod` 13 == 0]

--[1406357289,1430952867,1460357289,4106357289,4130952867,4160357289]
