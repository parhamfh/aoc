import System.IO
import Control.Monad
import Debug.Trace

import Split

main = do
    handle <- readFile "input.txt"
    let meridians = lines handle
    let filtered_meridian = second meridians
    let ct x = countTrees meridians x 0
    print $ (countTrees filtered_meridian 1 0) * (foldr1 (*) $ map ct [1,3,5,7])

countTrees :: [String] -> Int -> Int -> Int
countTrees [] slope x = 0
countTrees (m:mr) slope x
    | m !! x == '#' = 1 + (countTrees mr slope $ mod (x + slope) (length m))
    | otherwise     = countTrees mr slope $ mod (x + slope) (length m)

second :: [String] -> [String]
second (a:b:rest) = a:second rest
second _ = []
