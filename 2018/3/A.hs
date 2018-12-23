import Debug.Trace
import Data.List (groupBy)

-- ID - x0, y0, width, height
-- x from left edge
-- y from top edge
type Claim = (Int, Int, Int, Int, Int)

main = do
    handle <- readFile "input.txt"
    let claim_input = lines handle
    let claims = map (\x -> inputToClaims x) claim_input
    -- print $ last claims
    let s =  max (furthestDown claims + largestHeight claims) (furthestRight claims + largestWidth claims)
    print $ (squareMatrix 5) !! 2 !! 4
    print $ sharedSquares claims

sharedSquares :: [Claim] -> Int
sharedSquares claims = commonSquares claims $ squareMatrix $ max (furthestDown claims + largestHeight claims) (furthestRight claims + largestWidth claims)

commonSquares :: [Claim] -> [[Int]] -> Int
commonSquares claims matrix = 77

-- size of square matrix
squareMatrix :: Int -> [[Int]]
squareMatrix size = createMatrix size [[]] [map (* 0) [1..size]]

-- rows, initialized value , columns
createMatrix :: Int -> [[Int]] -> [[Int]] -> [[Int]]
createMatrix rows matrix columns
    | rows > 0      = createMatrix (rows-1) (matrix ++ columns) columns
    | rows == 0     = tail matrix

furthestDown :: [Claim] -> Int
furthestDown c = maximum $ map (claimVal 2) c

furthestRight:: [Claim] -> Int
furthestRight c = maximum $ map (claimVal 1) c

largestWidth :: [Claim] -> Int
largestWidth c = maximum $ map (claimVal 3) c

largestHeight :: [Claim] -> Int
largestHeight c = maximum $ map (claimVal 4) c

inputToClaims :: String -> Claim
inputToClaims s = (\[a,b,c,d,e] -> (a,b,c,d,e)) $ map stringToInt [
                    (tail $ tokens !! 0), 
                    (takeWhile (/= ',') $ tokens !! 2), 
                    (tail $ takeWhile (/= ':') $ dropWhile (/= ',') $ tokens !! 2), 
                    (takeWhile (/= 'x') $tokens !! 3),
                    (tail $ dropWhile (/= 'x') $tokens !! 3)
                  ]
    where
        tokens = groupBy (\a b -> b /= ' ') s

stringToInt :: String -> Int
stringToInt s = read s :: Int

-- ignore
claimVal :: Int -> Claim -> Int
claimVal index (a,b,c,d,e)
    | index == 0 = a
    | index == 1 = b
    | index == 2 = c
    | index == 3 = d
    | index == 4 = e

