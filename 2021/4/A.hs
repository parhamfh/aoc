import System.IO
import Control.Monad
import Debug.Trace

main = do
    handle <- readFile "input.txt"
    let ls = lines handle
    let num_str = head ls
    let numbers = numStr2Int num_str ""
    print numbers

    let board_str = tail $ tail ls
    print ""
    let boards = parseBoards board_str $ Board [] []
    print boards

    print ""
    let winner = firstWinner boards numbers [] 
    print winner
    print $ score winner


data Board = Board {
    rows :: [[Int]],
    cols :: [[Int]]
} deriving (Show)


numStr2Int :: String -> String -> [Int]
numStr2Int [] d = [read d]
numStr2Int (x:xs) d
    | x == ','  = [read d] ++ numStr2Int xs ""
    | otherwise = numStr2Int xs (d++[x])

--- Bingo

firstWinner :: [Board] -> [Int] -> [Int] -> (Board, [Int])
firstWinner b [] drawn = (Board [] [], [])
firstWinner b (n:ns) drawn
    | length winners >0     = (head winners, drawn)
    | otherwise             = firstWinner b ns (drawn++[n])
    where winners = filter (isWinner drawn) b


isWinner :: [Int] ->  Board -> Bool
isWinner nums b = (bingo (rows b) nums) || (bingo (cols b) nums)

bingo :: [[Int]] -> [Int] -> Bool
bingo [] nums = False
bingo (x:xs) nums
    | isbingo == True   = traceShow (x, nums) True
    | otherwise         = bingo xs nums
    where 
        isbingo = all (==True) numCheck
        numCheck = map (\t -> t `elem` nums) x

score :: (Board, [Int]) -> Int
score (b, drawn) = ((last drawn) *) $ sum $ filter (`notElem` drawn) $ concat $ rows b 

--- Parse boards

parseBoards ::  [String] -> Board -> [Board]
parseBoards [] b = [b] 
parseBoards (x:xs) b
    | x == ""   = [b] ++ (parseBoards xs $ Board [] [] )
    | otherwise = parseBoards xs b'
    where b' = addRow b (parseRow x "")


addRow :: Board -> [Int] -> Board
addRow b i = Board (rows b ++ [i]) (updateCols (cols b) i)


updateCols :: [[Int]] -> [Int] -> [[Int]]
updateCols _ [] = []
updateCols [] b = map (\t -> [t]) b
updateCols (x:xs) (b:bs) = [x++[b]] ++ (updateCols xs bs)


parseRow :: String -> String -> [Int] 
parseRow [] d = [read d]
parseRow (x:xs) d
    -- d can sometimes be an empty string if x is preceding space
    | x == ' ' && d /= ""  = [read d] ++ parseRow xs "" 
    | otherwise = parseRow xs (d++[x])
