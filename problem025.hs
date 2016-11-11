import Debug.Trace

-- fib 1 = 1
-- fib 2 = 1
-- fib n = if n `mod` 2 == 0 then trace ("fib " ++ (show n) ++ " = " ++ show (fib (n - 2) + fib (n - 1))) (fib (n - 2) + fib (n - 1))
--                             else fib (n - 2) + fib (n - 1)

fibs n 1 xs = if n == 1 then f else fibs n 2 [f]
  where f = 1
fibs n 2 xs = if n == 2 then f else fibs n 3 (f : xs)
  where f = 1
fibs n c xs = if n == c then f else fibs n (c + 1) (g : xs)
  where f = (head xs) + (head (tail xs))
        g = if (length $ show f) == 1000 then trace (show c) f else f

--main = putStr $ show $ takeWhile ((<= 1000) . length . show) $ map fib [1..]
main = putStr $ show $ fibs 100000 1 []
