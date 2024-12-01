import System.IO

import Debug.Trace

import Util

main = do
    -- handle <- readFile "input.txt"
    handle <- readFile "test.txt"
    let ls = lines handle
    -- get the part after ':'
    let cards = map (\x -> trim $ (split_on  x ':' "") !! 1) ls
    -- split the string in two at '|'
    let cards2 = map (\x -> map trim $ (split_on x '|' "")) cards
    -- parse the numbers as ints and remove the empty strings from the list
    let cards3 = [map (\x -> (map (\y -> read y ::Int)) $ (filter (\x -> x /= "")) $ split x "") c | c <- cards2]
    print $ cards3
    -- number of cards
    let scratch =  [0 | c <- [0..(length ls-1)]]

    -- reverse list
    let revcards = reverse cards3
    print revcards
    print scratch
    print $ countScratchCards revcards scratch
    -- print $ sum $ map scoreCard cards3 scratch

type Card = [[Int]]
countScratchCards :: [Card] -> [Int] -> Int
countScratchCards [] tally = sum tally
countScratchCards (c:cs) tally 
    |  

updateTally :: [Int] -> Int -> Int -> [Int]
updateTally list index val = fst split ++ [val] ++ (tail $ snd split)
    where split = splitAt index list

scoreCard :: [[Int]] -> Int
scoreCard (win:card)
    | points > 0 = 2^(points-1)
    | otherwise  = 0
    where 
        points = trace (show wins) length wins
        wins = filter (>0) [x | x <- card !! 0, x `elem` win]

