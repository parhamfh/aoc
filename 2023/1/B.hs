import System.IO
import Data.Char (ord)
import Data.List (elemIndex)
-- import Control.Monad
-- import Debug.Trace

main = do
    handle <- readFile "input.txt"
    -- handle <- readFile "test2.txt"
    let ls = lines handle
    print $ sum $ map findThem ls
    -- print $ sum . map (\x -> (x!!0)*10+(x!!1)) $ parse ls

vals = ["one", "two", "three", "four","five", "six", "seven", "eight", "nine", "1","2","3","4","5","6","7","8","9"]

findThem :: String -> Int
findThem s = first * 10 + last
    where 
        first = findFirst s vals
        last = findFirst (reverse s) (map reverse vals)

findFirst :: String -> [String]->  Int
findFirst s values =
    case true_index of
        Just x -> (x `mod` 9)+1
        _      -> findFirst (tail s) values 
     where true_index  = elemIndex True $ map (\x -> check x s) values 

check :: String -> String -> Bool
check val string
    | val == (take (length val) string) = True
    | otherwise = False
