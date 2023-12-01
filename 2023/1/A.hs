import System.IO
import Data.Char (ord)
-- import Control.Monad
-- import Debug.Trace

main = do
    handle <- readFile "input.txt"
    -- handle <- readFile "test.txt"
    let ls = lines handle
    print $ sum . map (\x -> (x!!0)*10+(x!!1)) $ parse ls

parse :: [String] -> [[Int]]
parse []     = []
parse (x:xs) = [sumDigits x [0,0]] ++ parse xs

sumDigits :: [Char] -> [Int] -> [Int]
sumDigits [] final = final 
sumDigits (x:xs) final
    | dVal > 0 && final==[0, 0] = sumDigits xs [dVal, dVal]
    | dVal > 0 = sumDigits xs [(final !! 0), dVal]
    | otherwise = sumDigits xs final
    where dVal = digValue x

digValue :: Char -> Int
digValue c
    | (ord c) > 47 && (ord c) < 58 = ord c - 48
    | otherwise = 0
-- isNumber :: Char -> Bool
-- isNumber c = check
    -- where check = c `elem` [0,1,2,3,4,5,6,7,8,9]
