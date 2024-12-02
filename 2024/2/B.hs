import System.IO
import Data.Char (ord, isSpace)
import Data.List (isPrefixOf, sort)

import Debug.Trace

main = do
    handle <- readFile "input.txt"
    -- handle <- readFile "demo.txt"
    --
    let ls = lines handle
    let reports = map toNum ls
    print reports
    let numSafe = filter isSafe reports
    print numSafe
    print $ length numSafe


toNum :: String -> [Int]
toNum s = map (\x -> read x :: Int) $ words s

isSafe :: [Int] -> Bool
isSafe ls 
    | isSafeDef ls = True
    | otherwise = damper ls [0..(length ls)-1]

damper :: [Int] -> [Int] -> Bool
damper ls [] = False
damper ls (ix:ixs)
    | isSafeDef (without ls ix) = True 
    | otherwise = damper ls ixs

without :: [Int] -> Int -> [Int]
without ls ix = (take ix ls) ++ (drop (ix+1) ls)

-- Give head ls+1 as arg which will be guaranteed pass condition 
isSafeDef :: [Int] -> Bool
isSafeDef ls = (isStable ls (<=)) || (isStable ls (>=))

isStable :: [Int] -> (Int -> Int -> Bool) -> Bool
isStable [a] cmp = True
isStable (a:b:as) cmp
    | (cmp b a) && (diff >= 1 && diff <= 3) = isStable (b:as) cmp
    | otherwise = False
    where diff= abs (b-a)

