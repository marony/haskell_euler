{-  10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1 = 3628800
    9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1 = 362880
    9文字目は 9! 周期で、0123456789(1～362880行目は'0'、362881～…'1')
    8文字目は 8! 周期で、0123456789(使われていない文字)
    7文字目は 7! 周期で、0123456789(使われていない文字)
    6文字目は 6! 周期で、0123456789(使われていない文字)
    5文字目は 5! 周期で、0123456789(使われていない文字)
    4文字目は 4! 周期で、0123456789(使われていない文字)
    3文字目は 3! 周期で、0123456789(使われていない文字)
    2文字目は 2! 周期で、0123456789(使われていない文字)
    1文字目は 1! 周期で、0123456789(使われていない文字)
    0文字目は余りの文字
-}

import Data.List
import Debug.Trace

list = ['0'..'9']

factorial 0 a = a
factorial n a = factorial (n - 1) a * n

{-
dicide_digit pos a 0 = trace ("pos = " ++ (show pos) ++ ", a = " ++ (show a)) a ++ (list \\ a)
-}
dicide_digit pos a 0 = a ++ (list \\ a)

{-
dicide_digit pos a n = dicide_digit (pos - 1) (a ++ [c]) (n - digit_cycle * digit_index)
    where digit_cycle = trace ("n = " ++ (show n) ++ ", pos = " ++ (show pos) ++ ", a = " ++ (show a) ++
            "\ndigit_cycle = " ++ (show $ factorial pos 1)) factorial pos 1
          digit_index = trace ("digit_index = " ++ (show $ n `div` digit_cycle)) n `div` digit_cycle
          chars = trace ("chars = " ++ (show $ list \\ a)) list \\ a
          c = trace ("char = " ++ (show $ chars !! digit_index)) chars !! digit_index
-}
dicide_digit pos a n = dicide_digit (pos - 1) (a ++ [c]) (n - digit_cycle * digit_index)
    where digit_cycle = factorial pos 1
          digit_index = n `div` digit_cycle
          chars = list \\ a
          c = chars !! digit_index

main = do putStrLn $ show $ map (dicide_digit ((length list) - 1) []) [1000000 - 1]
