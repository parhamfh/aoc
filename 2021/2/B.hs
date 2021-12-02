import System.IO
import Control.Monad
import Data.Typeable
import Debug.Trace

import Split

main = do
    handle <- readFile "input.txt"
    let meas =  lines handle
    print $ map parseMes meas
    let pos = calcPos $ reverse meas
    print pos
    let (x,y,aim) = pos
    print (x*y,aim)


calcPos :: [String] -> (Int, Int, Int)
calcPos []  = (0,0,0)
calcPos (x:xs)
    | "forward" == dir  = tupAdd (u, 0, 0) $ calcPos xs 
    | "down"    == dir  = tupAdd (0, 0, u) $ calcPos xs
    | "up"      == dir  = tupAdd (0, 0,-u) $ calcPos xs
    where 
        (dir, u) = parseMes x


parseMes :: String -> (String, Int)
parseMes m = (dir, u)
    where
        ml = split m []
        dir = head ml
        u = read $ ml !! 1

-- The list has to be reversed since the calculation is resolved from the right
-- (n+(..(d+(c+(b+a))
tupAdd :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
tupAdd (x,y,z) (a,b,c) = traceShow (a+x, b+(x*c), c+z)  (a+x, b+(x*c), c+z)
