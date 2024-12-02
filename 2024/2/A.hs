import System.IO
import Data.Char (ord, isSpace)
import Data.List (isPrefixOf, sort)

import Debug.Trace

main = do
    handle <- readFile "input.txt"
    -- handle <- readFile "test.txt"
    -- let ls = take 5 $ lines handle
    let ls = lines handle
    let reports = map toNum ls
    print reports
    let numSafe = filter isSafe reports
    print $ length numSafe


toNum :: String -> [Int]
toNum s = map (\x -> read x :: Int) $ words s

-- Give head ls+1 as arg which will be guaranteed pass condition 
isSafe :: [Int] -> Bool
isSafe ls = (isDecreasing ls (head ls + 1)) || (isIncreasing ls (head ls - 1))

isDecreasing :: [Int] -> Int -> Bool
isDecreasing [] _ = True
isDecreasing (a:as) prev
    | a <= prev && (diff >= 1 && diff <= 3) = isDecreasing as a
    | otherwise = False
    where diff= abs (a-prev)


isIncreasing :: [Int] -> Int -> Bool
isIncreasing [] _ = True
isIncreasing (a:as) prev
    | a >= prev && (diff >= 1 && diff <= 3) = isIncreasing as a
    | otherwise = False
    where diff= abs (a-prev)


