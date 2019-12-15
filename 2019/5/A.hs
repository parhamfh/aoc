import System.IO
import Control.Monad
import Debug.Trace
import Split

main = do
    handle <- readFile "input.txt"
    let instructions = lines handle
    let input = 1
    print $ runDiagnostics (splitComma (head instructions)) [1]

runDiagnostics :: [Int] -> [Int] -> [Int]
runDiagnostics instructions input = parseIntcodes instructions input

parseIntcodes :: [Int] -> [Int] -> [Int]
parseIntcodes ops input = execute 0 ops input []

-- see it as params in and then the list manipulated
execute :: Int -> [Int] -> [Int] -> [Int] -> [Int]
execute pos codes input output
    | inst == 1     = traceShow dbg execute next (executeAdd (params !! 0) (params !! 1) (params !! 2) codes) input output
    | inst == 2     = traceShow dbg execute next (executeMul (params !! 0) (params !! 1) (params !! 2) codes) input output
    | inst == 3     = traceShow dbg execute next (executeINPUT i (codes !! (params !! 0)) codes) input output 
    | inst == 4     = traceShow dbg execute next codes input (executeOUTPUT (params !! 0) output)
    | inst == 99    = traceShow ("Final code is: ", codes) output
    where
        (inst, modes) = parseInstruction pos codes
        (params, next) = getParams inst modes codes pos  
        i = head input
        dbg = ("Inst, modes, params, next: ", inst, modes, params, next)

parseInstruction :: Int -> [Int] -> (Int, [Int])
parseInstruction pos codes = (inst, modes) 
    where 
        (inst, modes) = parseIntcode inStr
        inStr = reverse (show (codes !! pos))

-- ugly , dont reverse r, instead parse ops string smarter - but should work for now
parseIntcode :: String -> (Int, [Int])
parseIntcode (dig:r)
    | dig == '1'    = (1, modes)
    | dig == '2'    = (2, modes)
    | dig == '3'    = (3, modes)
    | dig == '4'    = (4, modes)
    | dig == '9'    = (99, [])
    | otherwise     = (-1, [])
    where modes = getModes (tailGuard r)

getModes :: String -> [Int]
getModes s = pad2 (splitStr s)

pad2 :: [Int] -> [Int]
pad2 l
    | length l < 2  = l ++ (replicate (2-(length l)) 0)
    | otherwise     = l

getParams :: Int -> [Int] -> [Int] -> Int -> ([Int], Int)
getParams inst modes codes pos
    | inst == 1     = ((getInParams 2 modes codes pos) ++ [codes !! (pos+3)], pos+4) 
    | inst == 2     = ((getInParams 2 modes codes pos) ++ [codes !! (pos+3)], pos+4) 
    | inst == 3     = ([pos+1], pos+2) 
    | inst == 4     = ((getInParams 1 modes codes pos), pos+2)  
    | inst == 99    = ([],-777)
    | otherwise     = ([],-1)

getInParams :: Int -> [Int] -> [Int] -> Int -> [Int] 
getInParams n (mode:r) codes pos
    | n < 1         = []
    | mode == 0     = [codes !! (codes !! (pos + 1))] ++ getInParams (n-1) r codes (pos+1)
    | mode == 1     = [codes !! (pos + 1)] ++ getInParams (n-1) r codes (pos+1)
    | otherwise     = []
getInParams n [] codes pos = []

--- INSTRUCTIONS
executeAdd :: Int -> Int -> Int -> [Int] -> [Int]
executeAdd a b pos list = insertBetween (a+b) (splitAt pos list)

executeMul :: Int -> Int -> Int -> [Int] -> [Int]
executeMul a b pos list = insertBetween (a*b) (splitAt pos list)

executeINPUT :: Int -> Int -> [Int] -> [Int]
executeINPUT a pos list = insertBetween a (splitAt pos list)

insertBetween :: Int -> ([Int], [Int])  -> [Int]
insertBetween val (a, b) = a ++ [val] ++ tailGuard b

tailGuard :: [a] -> [a]
tailGuard b
    | null b    = []
    | otherwise = tail b

executeOUTPUT :: Int -> [Int] -> [Int]
executeOUTPUT a list = traceShow ("Output: ", a) output
    where output = list ++ [a]
