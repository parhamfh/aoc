import System.IO
import Control.Monad
import Data.Typeable
import Debug.Trace

import Split

main = do
    handle <- readFile "input.txt"
    let meas =  lines handle
    let pos = foldl1 (\acc x -> tupAdd acc x) $ calcPos meas
    print pos
    let (x,y,aim) = pos
    print (x*y,aim)


calcPos :: [String] -> [(Int, Int, Int)]
calcPos []  = [(0,0,0)]
calcPos (x:xs)
    | "forward" == dir  = [(u, 0, 0)] ++ calcPos xs 
    | "down"    == dir  = [(0, 0, u)] ++ calcPos xs
    | "up"      == dir  = [(0, 0,-u)] ++ calcPos xs
    where 
        (dir, u) = parseMes x


parseMes :: String -> (String, Int)
parseMes m = (dir, u)
    where
        ml = split m []
        dir = head ml
        u = read $ ml !! 1

tupAdd :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
tupAdd (a,b,c) (x,y,z) = (a+x, b+(x*c), c+z)
