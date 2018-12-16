import System.IO
import qualified Data.IntSet as IS
import Debug.Trace
import Data.List (intercalate)

main = do
    handle <- readFile "input.txt"
    let freqs = lines handle
    
    -- print all frequencies
    -- mapM print freqs
    print $ checkFreq 0 IS.empty freqs

checkFreq :: Int -> IS.IntSet -> [String] -> Int
checkFreq tot_freq freq_set [f]
    | t $ IS.member new_tot freq_set    = new_tot
    | otherwise                     = -1
    where
        new_tot = calcFreq tot_freq f
        t = trace (
                "calling checkFreq SINGLE with tot_freq = " ++ 
                    (show tot_freq :: String) ++ 
                " and freq_set is = " ++ 
                    (intercalate "," (map (\x -> (show (x) :: String)) (IS.elems freq_set)) ) 
            ) 
checkFreq tot_freq freq_set (f:f_remain)
    | t $ IS.member new_tot freq_set    = new_tot
    | t $ otherwise                     = checkFreq new_tot (IS.insert new_tot freq_set) f_remain
    where
        new_tot = calcFreq tot_freq f
        t = trace (
                "calling checkFreq LIST with tot_freq = " ++ 
                    (show tot_freq :: String) ++ 
                " and freq_set is = " ++ 
                    (intercalate "," (map (\x -> (show (x) :: String)) (IS.elems freq_set)) ) 
            ) 

calcFreq :: Int -> String -> Int
calcFreq total (sign:number)
    | sign == '+'   = t_add $ total + val
    | sign == '-'   = t_sub $ total - val
    where 
        val = (read number :: Int)
        t_add = trace ("addng val " ++ number ++ " to " ++ (show total) ++ " => " ++ (show $ total + val) ) 
        t_sub = trace ("subtr val " ++ number ++ " fr " ++ (show total) ++ " => " ++ (show $ total - val) )


