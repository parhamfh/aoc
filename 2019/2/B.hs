import System.IO
import Control.Monad
import Split

main = do
    handle <- readFile "input.txt"
    let instructions = lines handle
    print $ findValues (splitComma (head instructions))

findValues :: [Int] -> (Int, Int, Int)
findValues (a:b:c:rest) = iterateParams (a:0:0:rest)

iterateParams :: [Int] -> (Int, Int, Int)
iterateParams (a:noun:verb:rest)
    | head (parseIntcodes (a:noun:verb:rest)) == 19690720   = (noun, verb, 100*noun + verb)
    | noun <= 99                                            = iterateParams(a:(noun+1):verb:rest)
    | verb <= 99                                            = iterateParams(a:0:(verb+1):rest)
    | otherwise                                             = (-1, -1, -1)

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
