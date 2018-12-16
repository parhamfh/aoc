import System.IO
import qualified Data.IntSet as IS
import Debug.Trace
import Data.List (intercalate)

main = do
    handle <- readFile "input.txt"
    let freqs = lines handle
    
    -- print all frequencies
    -- mapM print freqs
    let freq_list = map (\x -> mapToInt x) freqs
    print $ show $ calibrate freq_list

calibrate :: [Int] -> Int
calibrate freqs = checkFreq 0 IS.empty [] freqs

checkFreq :: Int -> IS.IntSet -> [Int] ->  [Int] -> Int
checkFreq tot_freq freq_set next_set  [f]
    | IS.member new_tot freq_set    = new_tot
    | otherwise                     = checkFreq new_tot (IS.insert new_tot freq_set) [] (reverse (f:next_set)) 
    where
        new_tot = tot_freq + f

checkFreq tot_freq freq_set next_set (f:f_remain)
    | IS.member new_tot freq_set    = new_tot
    | otherwise                     = checkFreq new_tot (IS.insert new_tot freq_set) (f:next_set) f_remain
    where
        new_tot = tot_freq + f

mapToInt :: String -> Int
mapToInt (sign:number)
    | sign == '+'   = val
    | sign == '-'   = -1 * val
    where 
        val = (read number :: Int)

