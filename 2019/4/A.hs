-- import Control.Monad
import Data.Text.Internal.Read

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
isPassword i = checkValid (toList i) (-1) False

toList :: Int -> [Int]
toList digit = map digitToInt (show digit)

checkValid :: [Int] -> Int ->  Bool -> Bool
checkValid (i:r) prev hasDouble
    | i == prev     = checkValid r i True
    | i > prev      = checkValid r i hasDouble
    | otherwise     = False
checkValid [] prev hasDouble = hasDouble 
