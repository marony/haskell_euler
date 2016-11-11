sumDigits :: Integer -> Int
sumDigits n = sum $ map (\c -> read [c] :: Int) (show n)

main :: IO ()
main = putStrLn $ show $ maximum $ map sumDigits list
    where list = [x ^ y | x <- [1..99], y <- [1..99]]
