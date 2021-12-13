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
    -- print boards

    print ""
    let winners = allWinners boards numbers [] 
    let winner = head $ (drop 1) $ reverse winners
    print winner
    print ("winner was",length winners)
    print $ score winner


data Board = Board {
    rows :: [[Int]],
    cols :: [[Int]]
} deriving (Eq, Show)


numStr2Int :: String -> String -> [Int]
numStr2Int [] d = [read d]
numStr2Int (x:xs) d
    | x == ','  = [read d] ++ numStr2Int xs ""
    | otherwise = numStr2Int xs (d++[x])

--- Bingo

allWinners :: [Board] -> [Int] -> [Int] -> [(Board, [Int])]
allWinners b [] drawn = [((Board [] []), drawn)]
allWinners b (n:ns) drawn
    | length winners >0     = (map (\t -> (t, drawn)) winners) ++ (allWinners (removeWinners b winners) ns (drawn++[n]))
    | otherwise             = allWinners b ns (drawn++[n])
    where winners = filter (isWinner drawn) b

removeWinners :: [Board] -> [Board] -> [Board]
removeWinners b winners = filter (`notElem` winners) b

isWinner :: [Int] ->  Board -> Bool
isWinner nums b = (bingo (rows b) nums) || (bingo (cols b) nums)

bingo :: [[Int]] -> [Int] -> Bool
bingo [] nums = False
bingo (x:xs) nums
    | isbingo == True   = True
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
