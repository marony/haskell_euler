list = [1..100]
 
a = foldl (+) 0 $ map (\x -> x * x) list
b = c * c
    where c = foldl (+) 0 list
 
answer = b - a
 
main = putStr $ show answer
