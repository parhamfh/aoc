import System.IO
import Control.Monad
import Debug.Trace

main = do
    handle <- readFile "input.txt" 
    let bins = lines handle
    let len =  length $ head bins
    print len
    let gamma = getGamma bins [0..len-1]
    print gamma
    let epsilon = map (==False) gamma
    print epsilon
    print "Find oxygen \n" 
    let oxygen = findOxygen bins 0
    print oxygen
    let co2 = findCO2 bins 0
    print co2
    
    print $ (binMath (reverse $ bin2bool oxygen) 0) * (binMath (reverse $ bin2bool co2) 0)


findOxygen :: [String] -> Int -> String
findOxygen [bin] _  = bin
findOxygen bins col = traceShow (bins, col) findOxygen (filterBins bins col True) (col+1)

findCO2 :: [String] -> Int -> String
findCO2 [bin] _ = bin
findCO2 bins col = traceShow (bins, col) findCO2 (filterBins bins col False) (col+1)

filterBins :: [String] -> Int -> Bool -> [String]
-- if most is True filters for most common digit, if false, least common digit
filterBins bins col most
    | oneCommon == most = filter (\b -> (b !! col) == '1') bins
    | otherwise         = filter (\b -> (b !! col) == '0') bins
    where 
        oneCommon = isOneCommon bins col


------
bin2bool :: String -> [Bool]
bin2bool [] = []
bin2bool (b:bs)
    | b == '1'  = [True] ++ bin2bool bs
    | otherwise = [False] ++ bin2bool bs

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
    | countOnes bins col >= binLen   = True
    | otherwise                     = False
    where
        binLen = (length bins) `div` 2 + ((length bins) `mod` 2)


countOnes :: [String] -> Int -> Int
countOnes [] col = 0
countOnes (b:bs) col
    | b !! col == '1'   = 1 + countOnes bs col
    | otherwise         = countOnes bs col
