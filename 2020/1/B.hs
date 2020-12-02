import System.IO
import Control.Monad
import Debug.Trace

main = do
    handle <- readFile "input.txt"
    let expenses = map (read::String->Int) $ lines handle
    print $ find2020n 3 expenses


find2020n :: Int -> [Int] -> Int
find2020n n []  = error "Knas"
find2020n n (x:xs)
    | t  > 0    = t 
    | otherwise = find2020n n xs
    where
        t = testFriends (n-2) [x] xs 


testFriends :: Int -> [Int] -> [Int] -> Int
testFriends 0 friends expenses = test2020 friends expenses
testFriends numFiends friends [] = 0
testFriends numFriends friends (c:crest)
    | t > 0    = t 
    | otherwise    = testFriends numFriends friends crest
    where 
        t = testFriends (numFriends-1) (c:friends) crest


test2020 :: [Int] -> [Int] -> Int
test2020 p [] = 0
test2020 p (x:xs)
    | foldr1 (+) (x:p) == 2020   = traceShow ("found it "++show (x:p))foldr1 (*) (x:p)
    | otherwise                  = test2020 p xs
    
