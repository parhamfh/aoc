import System.IO
import Control.Monad

main = do
    handle <- readFile "input.txt"
    let modules = lines handle
    
    print $ moduleMass modules

moduleMass :: [String] -> Int
moduleMass [m] = calcMass m
moduleMass (m : m_remain) = calcMass m + moduleMass m_remain

calcMass :: String -> Int 
calcMass m = div (read m :: Int) 3 - 2
