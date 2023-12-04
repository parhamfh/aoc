import System.IO
import Data.Char (ord, isSpace)
import Data.List (isPrefixOf)

import Debug.Trace

import Split

main = do
    handle <- readFile "input.txt"
    -- handle <- readFile "test.txt"
    let ls = lines handle
    print $ sum $ filter (\x -> x >0) $ map (\x -> read x :: Int) $ map validateGame $ ls
    -- print $ validateGame $ ls !! 0

-- return id of game if valid, otherwise minus 1
validateGame :: String -> String
validateGame s
    | validGame gameRes = gameId
    |  otherwise = "-1"
    where 
        gameId = reverse $ tail $ reverse $ gameIdStr
        gameIdStr = gameStr !! 1
        gameRes = tail $ tail $ gameStr
        gameStr = (split s "") 

validGame :: [String] -> Bool
validGame gs = parsedRes
    where 
        -- s = trace (unwords parsedRes) True
        parsedRes = not (False `elem` (parseGameRes (unwords gs)))


parseGameRes :: String -> [Bool]
parseGameRes gs = map validateRoundStr (split_on gs ';' "")

validateRoundStr :: String -> Bool
validateRoundStr gs 
    | valid = True
    | otherwise = False
    where
        valid = trace (show dices) validDices dices
        dices = map trim (split_on gs ',' "")

-- 12 red cubes, 13 green cubes, and 14 blue 
validDices :: [String] -> Bool
validDices [] = True
validDices (dice_str:rest)
    | color == "blue" && value <= 14    = validDices rest
    | color == "red" && value <= 12     = validDices rest
    | color == "green" && value <= 13   = validDices rest 
    | otherwise =  False
    where 
        value = read (dice !! 0) :: Int
        color = dice !! 1 
        dice = split dice_str ""


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace    
