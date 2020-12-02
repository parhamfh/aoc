import System.IO
import Control.Monad
import Debug.Trace

import Split

main = do
    handle <- readFile "input.txt"
    let passwords = lines handle

    print $ countValidPw passwords


countValidPw :: [String] -> Int
countValidPw [] = 0
countValidPw (pw:pws)
    | traceShow ("agda "++pw) validPw pw    = 1 + countValidPw pws
    |  otherwise     = countValidPw pws


validPw :: String -> Bool
validPw pw_str
    | checkPolicy low high char pw  = True
    | otherwise                     = False
    where (low,high,char,pw) = parsePolicy pw_str

parsePolicy :: String -> (Int, Int, Char, String)
parsePolicy pol = (bounds !! 0, bounds !! 1, (head $ pol_str !! 1), pol_str !! 2)
    where
        pol_str = split pol ""
        bounds = sp_ci_on (pol_str !! 0) '-' ""

-- traceShow (show (low,high, c, pw)) 
checkPolicy :: Int -> Int -> Char -> String -> Bool
checkPolicy low high c pw 
    | ((pw !! (low-1)) == c) /= ((pw !! (high-1)) == c) = traceShow "true" True
    | otherwise                                     = traceShow "false" False
