import Data.List (intersect)
import Debug.Trace

main = do
    handle <- readFile "input.txt"
    let ids = lines handle
    print $ common_for_close ids

common_for_close :: [String] -> String
common_for_close (id:ids) = find_close id ids

find_close :: String -> [String] -> String 
find_close id [i] = ""
find_close id (i:ids)
    | close False id i      = intersect id i
    | otherwise             = find_close id ids

close :: Bool -> String -> String -> Bool
close one_diff (a:as) (b:bs)
    | a == b                            = close one_diff as bs
    | a /= b && (one_diff == True)      = False
    | a /= b && (one_diff == False)     = close True as bs
    | otherwise                         = True

