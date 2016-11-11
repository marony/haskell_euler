isPalindrome :: Integer -> Bool
isPalindrome n = (show n) == (reverse $ show n)

reverseNumber :: Integer -> Integer
reverseNumber n = read (reverse $ show n) :: Integer

maxCount = 50

makePalindrome :: Int -> Integer -> Bool
makePalindrome c n | c > maxCount   = False
                   | isPalindrome a = True
                   | otherwise      = makePalindrome (c + 1) a
    where a = n + reverseNumber n

main :: IO ()
main = putStrLn $ show $ length $ filter (not . (makePalindrome 1)) [1..10000]

