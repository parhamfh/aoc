import System.IO
import Control.Monad
import Debug.Trace

import Split


main = do
    handle <- readFile "input.txt"
    let answers = lines handle
    print $ sum $ map length $ uniq $ group answers []
    
group :: [String] -> [String] -> [[String]]
group [] a = [a]
group (a:as) ans
    | a == ""      = [ans] ++ group as []
    | otherwise     = group as (ans++[a])


uniq :: [[String]] -> [String]
uniq [] = []
uniq (a:as) = (prune (foldr1 (++) a) ""): uniq as

prune :: String -> String -> String
prune [] s = s
prune (a:as) s
    | elem a s  = prune as s
    | otherwise = prune as $ a:s

