import System.IO
import Control.Monad

main = do
    handle <- readFile "input.txt"
    let bins = lines handle
    let len =  length $ head bins
    print len
    let gamma = getGamma bins [0..len-1]
    print gamma
    let epsilon = map (==False) gamma
    -- print epsilon
    print $ (binMath (reverse gamma) 0) * (binMath (reverse epsilon) 0)


binMath :: [Bool] -> Int -> Int
binMath [] exponent = 0
binMath (d:ds) exponent = x * 2^exponent + binMath ds (exponent+1)
    where x = bool2dig d


bool2dig :: Bool -> Int
bool2dig b
    | b == True = 1
    | otherwise = 0

-- So, the gamma rate is the binary number 10110, or 22 in decimal.
-- So, the epsilon rate is 01001, or 9 in decimal.

getGamma :: [String] -> [Int] -> [Bool]
getGamma bins [x]    = [isOneCommon bins x]
getGamma bins (i:ix) = [isOneCommon bins i] ++ getGamma bins ix


isOneCommon :: [String] -> Int -> Bool
isOneCommon bins col
    | countOnes bins col > binLen   = True
    | otherwise                     = False
    where
        binLen = (length bins) `div` 2


countOnes :: [String] -> Int -> Int
countOnes [] col = 0
countOnes (b:bs) col
    | b !! col == '1'   = 1 + countOnes bs col
    | otherwise         = countOnes bs col
