import System.IO
import Control.Monad

main = do
    handle <- readFile "input.txt"
    let meas = map read $ lines handle
    print $ meas 
    print $ countInc meas

countInc :: [Int] -> Int
countInc [x] = 0
countInc (x:y:xs)
    | x < y     = 1 + countInc (y:xs)
    | otherwise = countInc (y:xs)
