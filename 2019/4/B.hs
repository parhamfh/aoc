import Data.Text.Internal.Read
import Data.List

main = do
    print $ numPasswords 284639 748759


numPasswords :: Int -> Int -> Int
numPasswords start end = length (findPasswords start end)

findPasswords :: Int -> Int -> [Int]
findPasswords current end
    | isPassword current && current < end   = [current] ++ (findPasswords (current+1) end)
    | isPassword current                    = [current]
    | current < end                         = findPasswords (current+1) end 
    | otherwise                             = []

isPassword :: Int -> Bool
isPassword i = checkValid (toList i) (-1) []

toList :: Int -> [Int]
toList digit = map digitToInt (show digit)

checkValid :: [Int] -> Int -> [Int] -> Bool 
checkValid (i:r) prev multips
    | i == prev        = checkValid r i (multips ++ [i])
    | i > prev         = checkValid r i multips 
    | otherwise     = False
checkValid [] prev multips = elem 1 (map length (group multips)) 
