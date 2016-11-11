{-
f :: Int -> Int
f 0 = 1
f 1 = 2
f n = 2 * f (n - 1) + f (n - 2)

fa :: Int -> Int
fa 1 = 3
fa n = (f n) + f (n - 1)

fb :: Int -> Int
fb 1 = 2
fb n = f n
-}

fs :: Int -> [Integer] -> [Integer]
fs 0 xs = [1]
fs 1 xs = [2, 1]
fs n xs = 2 * (fss !! 0) + (fss !! 1) : fss
    where fss = fs (n - 1) xs

fa :: Int -> Integer
fa n = (fs n []) !! 0 + (fs (n - 1) []) !! 0

fb :: Int -> Integer
fb n = (fs n []) !! 0

main :: IO ()
main = putStrLn $ show $ length $ filter (\(a, b) -> (length $ show a) > (length $ show b)) [(fa n, fb n) | n <- [1..1000]]

