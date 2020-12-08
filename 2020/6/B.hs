import System.IO
import Control.Monad
import Debug.Trace

import Split


main = do
    handle <- readFile "input.txt"
    let answers = lines handle
    print $ sum $ consensus $ group answers []


group :: [String] -> [String] -> [[String]]
group [] a = [a]
group (a:as) ans
    | a == ""      = [ans] ++ group as []
    | otherwise     = group as (ans++[a])


consensus :: [[String]] -> [Int]
consensus [] = []
consensus (g:gs) = [unanimous u g (length g)] ++ consensus gs
    where joint = foldr1 (++) g
          u = uniq joint ""


uniq :: String -> String -> String
uniq [] s = s
uniq (a:as) s
    | elem a s  = uniq as s
    | otherwise = uniq as $ a:s

 
-- g - contains the individual members answers
-- u - the string that combines all the distinct answers of the group
--
-- for every distinct answer, does it exist in all group member's answers?

unanimous :: String -> [String] -> Int -> Int
unanimous [] g target = 0
unanimous (u:us) g target
    | numYes == target  = 1 + (unanimous us g target)
    | otherwise         = unanimous us g target
    where numYes = length $ filter (==True) $ map (elem u) g

