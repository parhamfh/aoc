import System.IO
import Control.Monad
import Debug.Trace 

main = do
    handle <- readFile "input.txt"
    let modules = lines handle
    
    print $ moduleFuel modules

moduleFuel :: [String] -> Int
moduleFuel [m] = totalFuel m
moduleFuel (m : m_remain) = totalFuel m + moduleFuel m_remain

totalFuel :: String -> Int
totalFuel m_str = calcTotalFuel (read m_str :: Int)

calcTotalFuel :: Int -> Int
calcTotalFuel m 
    | m < 9 = 0
    | otherwise = calcFuel m + calcTotalFuel (calcFuel m) 


calcFuel :: Int -> Int 
calcFuel m = div m 3 - 2
