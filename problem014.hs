-- ghc --make problem014.hs -with-rtsopts="-H128m -K64m"

import Data.List (sort)

collatz 1 a = a + 1
collatz n a = collatz r (a + 1)
  where r = if (n `mod` 2) == 0 then
              n `div` 2
            else
              3 * n + 1

main = putStr $ show $ maximum $ map (\x -> (collatz x 0, x)) [1..1000000]
