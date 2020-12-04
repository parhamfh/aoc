import System.IO
import Control.Monad
import Debug.Trace

import Split


main = do
    handle <- readFile "input.txt"
    let passport_batch = lines handle
    print $ sum $ map validatePassport $ passportInfo passport_batch []

passportInfo :: [String] -> [String] -> [[String]]
passportInfo [] pass = [pass]
passportInfo (pi:pr) pass
    | pi == ""      = [pass] ++ passportInfo pr []
    | otherwise     = passportInfo pr (pass++[pi])

validatePassport :: [String] -> Int
validatePassport p
    | numValidFields == 7   = 1
    | otherwise             = 0
    where numValidFields = (checkPassportFields . parsePassport) p

parsePassport :: [String] -> [String]
parsePassport [] = []
parsePassport (p:pr) = (map (take 3) $ split p []) ++ parsePassport pr

checkPassportFields :: [String] -> Int
checkPassportFields [] = 0
checkPassportFields (f:fr)
    | elem f ["byr", 
              "iyr",
              "eyr",
              "hgt",
              "hcl",
              "ecl",
              "pid" ] = 1 + checkPassportFields fr
    | otherwise = checkPassportFields fr
