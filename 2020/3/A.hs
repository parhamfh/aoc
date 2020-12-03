import System.IO
import Control.Monad
import Debug.Trace

import Split

main = do
    handle <- readFile "input.txt"
    let meridians = lines handle

    print $ countTrees meridians 0 

countTrees :: [String] -> Int -> Int
countTrees [] x = 0
countTrees (m:mr) x
    | m !! x == '#' = 1 + (countTrees mr $ mod (x+3) (length m))
    | otherwise     = countTrees mr $ mod (x+3) (length m)
