import System.IO
import Control.Monad

main = do
    handle <- readFile "input.txt"
    let freqs = lines handle
    
    -- print all frequencies
    -- mapM print freqs
    print $ countFreq freqs

countFreq :: [String] -> Int
countFreq [f] = calcFreq f
countFreq (f: f_remain) = calcFreq f + countFreq f_remain

calcFreq :: String -> Int
calcFreq (sign:number)
    | sign == '+'   = read number :: Int
    | sign == '-'   = -1 * read number :: Int
