import System.IO
import Data.Char (ord, isSpace)
import Data.List (isPrefixOf)

import Debug.Trace

import Split

main = do
    handle <- readFile "input.txt"
    -- handle <- readFile "test.txt"
    let ls = lines handle
    -- get the part after ':'
    let cards = map (\x -> trim $ (split_on  x ':' "") !! 1) ls
    -- split the string in two at '|'
    let cards2 = map (\x -> map trim $ (split_on x '|' "")) cards
    -- parse the numbers as ints and remove the empty strings from the list
    let cards3 = [map (\x -> (map (\y -> read y ::Int)) $ (filter (\x -> x /= "")) $ split x "") c | c <- cards2]
    print $ cards3
    -- print $ sum $ filter (\x -> x >0) $ map (\x -> read x :: Int) $ map validateGame $ ls
    -- print $ sum $ map validateGame ls
    print $ sum $ map scoreCard cards3

scoreCard :: [[Int]] -> Int
scoreCard (win:card)
    | points > 0 = 2^(points-1)
    | otherwise  = 0
    where 
        points = trace (show wins) length wins
        wins = filter (>0) [x | x <- card !! 0, x `elem` win]


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace    
