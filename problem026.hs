import Data.List
import Debug.Trace

{- recurring_cycle
   小数の循環節の長さを返す
       割られる数
       割る数
       循環節の長さ -}
recurring_cycle :: (Integral a, Show a) => a -> a -> Int
recurring_cycle d1 d2 = recurring_cycle' d1 d2 []
    where recurring_cycle' :: (Integral a, Show a) => a -> a -> [a] -> Int
          recurring_cycle' d1 d2 ms = case modulo of
                                          0 -> 0
                                          _ -> recurring_cycle'' d1 d2 ms division modulo
              where a = divMod d1 d2
                    division = fst a
                    modulo = snd a
                    recurring_cycle'' :: (Integral a, Show a) => a -> a -> [a] -> a -> a -> Int
                    recurring_cycle'' d1 d2 ms division modulo = case result of
                                                                     Just x  -> x + 1
                                                                     Nothing -> recurring_cycle' (modulo * 10) d2 (modulo : ms)
                    result = elemIndex modulo ms

compare_tupple p x y = compare (p x) (p y)

list = [1..999]

main :: IO ()
main = putStr $ show $ maximumBy (\x y -> compare (snd x) (snd y)) $ zip list (map (recurring_cycle 1) list)
