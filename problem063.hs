lengthDigit :: Integral a => a -> a
lengthDigit n = lengthDigit' n 1
    where lengthDigit' :: Integral a => a -> a -> a
          lengthDigit' n c | n < 10    = c
                           | otherwise = lengthDigit' (n `div` 10) (c + 1)

main :: IO ()
main = putStrLn $ show $ length list
    where list = [(x ^ y, x, y) | y <- [1..21], x <- [1..9],
           (length $ show $ x ^ y) == y]
