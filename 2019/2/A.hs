import System.IO
import Control.Monad
import Split

main = do
    handle <- readFile "input.txt"
    let instructions = lines handle
    print $ processIntcode (head instructions)

processIntcode :: String -> [Int]
processIntcode insString = parseIntcodes (splitComma insString)

parseIntcodes :: [Int] -> [Int]
parseIntcodes ops  = execute 0 ops

-- see it as params in and then the list manipulated
execute :: Int -> [Int] -> [Int]
execute pos codes 
    | codes !! pos == 1     = execute (pos + 4) (executeAdd (paramsAt pos codes) (codes !! (pos + 3)) codes)
    | codes !! pos == 2     = execute (pos + 4) (executeMul (paramsAt pos codes) (codes !! (pos + 3)) codes)
    | codes !! pos == 99    = codes
    | otherwise             = codes

paramsAt :: Int -> [Int] -> (Int, Int) 
paramsAt pos list = (list !! (list !! (pos + 1)), list !! (list !! (pos + 2)))

executeAdd :: (Int, Int) -> Int -> [Int] -> [Int]
executeAdd (a, b) pos list = insertBetween (a+b) (splitAt pos list)

executeMul :: (Int, Int) -> Int -> [Int] -> [Int]
executeMul (a, b) pos list = insertBetween (a*b) (splitAt pos list)

insertBetween :: Int -> ([Int], [Int])  -> [Int]
insertBetween val (a, b) = a ++ [val] ++ tail b
