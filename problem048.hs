main :: IO ()
main = putStr $ show $ foldl (\a x -> a + x ^ x) 0 [1..1000]
