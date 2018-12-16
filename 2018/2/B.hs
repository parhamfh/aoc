import Data.List (intersect)
import Debug.Trace

main = do
    handle <- readFile "input.txt"
    let ids = lines handle
    print $ common_for_close ids

common_for_close :: [String] -> String
common_for_close (id:ids) = find_close id [] ids

find_close :: String -> [String] -> [String] -> String 
find_close id [] [i] = ""
find_close id (i_other:other) [i] = find_close i_other [] other
find_close id other_ids (i:ids)
    | close False id i      = intersect id i
    | otherwise             = find_close id (i:other_ids) ids

close :: Bool -> String -> String -> Bool
close one_diff [] []                    = True
close one_diff (a:as) (b:bs)
    | a == b                            = close one_diff as bs
    | a /= b && (one_diff == True)      = False
    | a /= b && (one_diff == False)     = close True as bs

