import System.IO
import Control.Monad
import Debug.Trace

import Split


main = do
    handle <- readFile "input.txt"
    let seats = lines handle
    print $ maximum $ map seatId seats
    
seatId :: String -> Int
seatId s = 8 * row + column
    where row = seatRow (take 7 s) 128
          column = seatCol (drop 7 s) 8

seatRow :: String -> Int -> Int
seatRow [] row = 0
seatRow (r:rs) rows
    | r == 'F'  = seatRow rs $ rows `div` 2
    | r == 'B'  = (rows `div` 2) + (seatRow rs $ rows `div` 2)

seatCol :: String -> Int -> Int
seatCol [] col = 0
seatCol (c:cs) cols
    | c == 'L'  = seatCol cs $ cols `div` 2
    | c == 'R'  = (cols `div` 2) + (seatCol cs $ cols `div` 2)
