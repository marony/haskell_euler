import Data.Char
import Data.Bits
import Text.Regex
import Data.List
import Text.ParserCombinators.Parsec
import Debug.Trace

dataFile :: CharParser st [[String]]
dataFile = endBy line eol
line :: CharParser st [String]
line = sepBy cell (char ',')
cell :: CharParser st String
cell = many (noneOf ",\n")
eol :: CharParser st Char
eol = char '\n'

judge :: String -> Maybe [String]
judge [] = Nothing
judge xs = matchRegex (mkRegex "^[ -~]+$") xs

ratio :: String -> Double
ratio s = ratio' s 0 0.0
    where ratio' :: String -> Int -> Double -> Double
          ratio' []     l r = r / (fromIntegral l :: Double)
          ratio' (x:xs) l r = case matchRegex (mkRegex "[a-zA-Z ]") [x] of
                              Just x' -> ratio' xs (l + 1) (r + 1.0)
                              _       -> ratio' xs (l + 1) r

process :: [Int] -> String -> Maybe (Double, String)
process codes password = case judge s of
                         Just _ -> Just (ratio s, s)
                         _      -> Nothing
    where process' xs [] = process' xs password
          process' [x] (y:ys)    = [chr $ x `xor` (ord y)]
          process' (x:xs) (y:ys) = (chr $ x `xor` (ord y)) : process' xs ys
          s = process' codes password

main :: IO()
main = do
    cs <- getContents
    let res = parse dataFile "" cs
    case res of
        Left err -> print err
        Right xs -> putStrLn $ show $ strToNum answer
            where codes = map (\x -> read x :: Int) $ concat xs
                  passwords = [a : b : c : [] | a <- ['a'..'z'], b <- ['a'..'z'], c <- ['a'..'z']]
                  answer = maximum $ filter (/= Nothing) $ map (process codes) passwords
                  strToNum (Just (r, s)) = foldl (\x y -> x + ord y) 0 s
