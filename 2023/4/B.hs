import System.IO

import Debug.Trace

import Util

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

    -- reverse list
    print $ countScratchCards cards3
    -- print $ sum $ map scoreCard cards3 scratch

type Card = [[Int]]
countScratchCards :: [Card] -> Int
countScratchCards [] = 0
countScratchCards (c:cs) = 1+  (scoreCard c cs)  + countScratchCards cs

scoreCard :: Card -> [Card] -> Int
scoreCard (win:card) cs
    | points > 0 = points + (sum [scoreCard (cs !! i) (drop (i+1) cs) | i <- [0..(points-1)]])
    | otherwise  = 0
    where
        points =length wins
        -- !! 0 because card is a list of the rest of elements (only 1 here)
        wins = filter (>0) [x | x <- card !! 0, x `elem` win]

