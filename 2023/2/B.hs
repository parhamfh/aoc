import System.IO
import Data.Char (ord, isSpace)
import Data.List (isPrefixOf)

import Debug.Trace

import Split

main = do
    handle <- readFile "input.txt"
    -- handle <- readFile "test.txt"
    let ls = lines handle
    -- print $ sum $ filter (\x -> x >0) $ map (\x -> read x :: Int) $ map validateGame $ ls
    print $ sum $ map validateGame ls

-- return id of game if valid, otherwise minus 1
validateGame :: String -> Int
validateGame s = calcPower gameRes
    where 
        gameId = reverse $ tail $ reverse $ gameIdStr
        gameIdStr = gameStr !! 1
        gameRes = tail $ tail $ gameStr
        gameStr = (split s "") 

calcPower :: [String] -> Int
calcPower gs = lowBase
    where 
        lowBase = trace (show finalBase) mulBase finalBase
        finalBase = trace (show parsedBases) mergeBases parsedBases (0,0,0)
        -- s = trace (unwords parsedRes) True
        parsedBases = parseGameRes (unwords gs)

type PowerBase = (Int, Int, Int)

mulBase :: PowerBase -> Int
mulBase (red, green, blue) = red * green * blue

mergeBases :: [PowerBase] -> PowerBase -> PowerBase
mergeBases [] pb = pb
mergeBases ((m,n,o):xs) (r,g,b) = mergeBases xs (large r m, large g n, large b o) 
 
large :: Int -> Int -> Int
large a b
    | a == 0    = b
    | b == 0    = a
    | a > b    = a
    | otherwise = b
    
parseGameRes :: String -> [PowerBase]
parseGameRes gs = map getBase (split_on gs ';' "")

getBase :: String -> PowerBase
getBase gs = pb
    where
        -- pb = trace (show gameRes) findBase gameRes (0,0,0)
        pb = findBase gameRes (0,0,0)
        gameRes = map trim (split_on gs ',' "")

-- 12 red cubes, 13 green cubes, and 14 blue 
findBase :: [String] -> PowerBase ->  PowerBase
findBase [] pb = pb
findBase (dice_str:rest) (red, green, blue)
    | color == "blue"    = findBase rest (red, green, value)
    | color == "red"     = findBase rest (value, green, blue)
    | color == "green"   = findBase rest (red, value, blue)
    where 
        value = read (dice !! 0) :: Int
        color = dice !! 1 
        dice = split dice_str ""


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace    
