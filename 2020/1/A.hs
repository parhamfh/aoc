import System.IO
import Control.Monad
import Debug.Trace

main = do
    handle <- readFile "input.txt"
    let expenses = lines handle

    print $ find2020 expenses

find2020 :: [String] -> Int
find2020 [x] = error "Knas"
find2020 (ex1 : exs)
    | t > 0     = t
    | otherwise = find2020 exs
    where 
        a = read ex1 :: Int 
        t = test2020 a exs

test2020 :: Int -> [String] -> Int
test2020 p [] = 0
test2020 p (x:xs)
    | p + a == 2020 = p * a
    | otherwise     = test2020 p xs
    where a = read x :: Int
