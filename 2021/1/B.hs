import System.IO
import Control.Monad

main = do
    handle <- readFile "input.txt"
    let meas = map read $ lines handle
    print $ threeSum meas 
    print $ countInc $ threeSum meas

threeSum :: [Int] -> [Int]
threeSum [_, _] = []
threeSum (x:y:z:xs) = [x+y+z] ++ threeSum (y:z:xs)

countInc :: [Int] -> Int
countInc [x] = 0
countInc (x:y:xs)
    | x < y     = 1 + countInc (y:xs)
    | otherwise = countInc (y:xs)
