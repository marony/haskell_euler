-- → = 0, ↓ = 1とすると、
-- ex) SIZE = 2
-- 0011
-- 0101
-- 0110
-- 1001
-- 1010
-- 1100
-- の6通り
-- 2 ^ (SIZE * 2)通りの2ビット数のうち
-- ビットの0か1がSIZE個を超えるものを削る
-- → それ以上右や下にはいけないため
-- ex) 0000, 0001, 1110, 1111

-- 2x2 => 6
-- 3
-- 3x3 => 20
-- 10
-- 4x4 => 70
-- 35
-- 5x5 => 252
-- 126
-- 6x6 => 924
-- 462

-- x / Cn = (n + 1)

-- process 0 0 n = n + 1
-- process 0 _ n = n + 1
-- process _ 0 n = n + 1
-- process x y n = xn + yn
--   where xn = if x > 0 then process (x - 1) y n else n
--         yn = if y > 0 then process x (y - 1) n else n
-- 
-- main = putStr $ show $ process 20 20 0

factor 0 = 1
factor n = n * factor (n - 1)

catalan_number n = (factor (2 * n)) `div` ((factor (n + 1)) * (factor n))

answer n = (n + 1) * (catalan_number n)

main = putStr $ show $ (!! (n - 1)) . take n $ map answer [1..]
  where n = 20