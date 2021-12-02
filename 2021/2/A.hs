import System.IO
import Control.Monad
import Data.Typeable

import Split

main = do
    handle <- readFile "input.txt"
    let meas =  lines handle
    let pos = calcPos meas
    print pos
    let (x,y) = pos
    print (x*y)


calcPos :: [String] -> (Int, Int)
calcPos []  = (0,0)
calcPos (x:xs)
    | "forward" == dir  = tupAdd (u, 0)  $ calcPos xs 
    | "down"    == dir  = tupAdd (0, u) $ calcPos xs
    | "up"      == dir  = tupAdd (0, -u) $ calcPos xs
    where 
        (dir, u) = parseMes x


parseMes :: String -> (String, Int)
parseMes m = (dir, u)
    where
        ml = split m []
        dir = head ml
        u = read $ ml !! 1

tupAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
tupAdd (a,b) (c,d) = (a+c, b+d)
