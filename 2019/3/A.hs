import System.IO
import Control.Monad
import Debug.Trace
import Split

main = do
    handle <- readFile "input.txt"
    let paths = lines handle
    let a = head paths
    let b = paths !! 1
    -- print $ findCrossing 
    print $ distanceToCrossing a b

-- (Direction, Distance)
type Direction = (Char, Int)

distanceToCrossing :: String -> String -> Int
distanceToCrossing pathA pathB = traceShow (distances) foldr1 min distances 
    where distances = map manhattanDistance (findCrossings (pointsOnPath pathA) (pointsOnPath pathB)) 
    
pointsOnPath :: String -> [(Int, Int)]
pointsOnPath path = calcDirections (0,0) (map parseDirection (splitComma path))

parseDirection :: String -> Direction
parseDirection str = d
    where d = ((head str), (read (tail str) :: Int))

calcDirections :: (Int, Int) -> [Direction] -> [(Int, Int)]
calcDirections (x,y) ((dir, dis):rest)
    | dir == 'U' = pathUp ++ calcDirections (last pathUp) rest  
    | dir == 'D' = pathDown ++ calcDirections (last pathDown) rest
    | dir == 'R' = pathRight ++ calcDirections (last pathRight) rest
    | dir == 'L' = pathLeft ++ calcDirections (last pathLeft) rest
    where 
        start       = (x,y)
        pathUp      = countUp start dis
        pathDown    = countDown start dis
        pathRight   = countRight start dis
        pathLeft    = countLeft start dis
calcDirections (x, y) [] = []

countUp :: (Int, Int) -> Int -> [(Int, Int)]
countUp (x,y) distance 
    | distance > 0  = [next] ++ countUp next (distance-1)
    | otherwise     = []
    where next = (x, y+1)

countDown :: (Int, Int) -> Int -> [(Int, Int)]
countDown (x,y) distance
    | distance > 0  = [next] ++ countDown next (distance-1)
    | otherwise     = []
    where next = (x, y-1)

countRight :: (Int, Int) -> Int -> [(Int, Int)]
countRight (x,y) distance
    | distance > 0  = [next] ++ countRight next (distance-1)
    | otherwise     = []
    where next = (x+1, y)

countLeft :: (Int, Int) -> Int -> [(Int, Int)]
countLeft (x,y) distance
    | distance > 0  = [next] ++ countLeft next (distance-1)
    | otherwise     = []
    where next = (x-1, y)

findCrossings :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
findCrossings (firstPointA:restPointsA) pointsB
    | pointsInPath firstPointA pointsB  = [firstPointA] ++ findCrossings restPointsA pointsB
    | otherwise                         = findCrossings restPointsA pointsB
findCrossings [] pointsB = []

pointsInPath :: (Int, Int) -> [(Int, Int)] -> Bool
pointsInPath (x, y) ((x2,y2):r)
    | x == x2 && y == y2    = traceShow ("Point " ++ show (x,y) ++ " was in path b" ) True
    | r == []               = False
    | otherwise             = pointsInPath (x, y) r

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x,y) = (abs x) + (abs y)
