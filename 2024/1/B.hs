import System.IO
import Data.Char (ord, isSpace)
import Data.List (isPrefixOf, sort)

import Debug.Trace

import Split

main = do
    handle <- readFile "input.txt"
    -- handle <- readFile "test.txt"
    -- let ls = take 5 $ lines handle
    let ls = lines handle
    print "adfa"
    let pairs = map ((map (\x -> read x :: Int)) . words) ls
    print pairs
    let (alist, blist) = splitList pairs [] []
    let alist2 = sort alist
    let blist2 = sort blist
    print $ sum $ diff alist2 blist2
    print $ sum $ map (\x -> x * (count x blist2)) alist2

splitList :: [[Int]] -> [Int] -> [Int] -> ([Int], [Int])
splitList [] llist rlist    = (llist, rlist)
splitList (a:b) llist rlist = splitList b (llist ++ [(a !! 0)]) (rlist ++ [(a !! 1)])

diff :: [Int] -> [Int] -> [Int]
diff [] [] = [0]
diff (a:as) (b:bs) = [abs (a-b)] ++  (diff as bs)

count :: Int -> [Int] -> Int
count t ls = (length . (filter (==t))) ls
