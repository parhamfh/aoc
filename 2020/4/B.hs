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

parsePassport :: [String] -> [(String, String)]
parsePassport [] = []
parsePassport (p:pr) = (map splitToken $ split p []) ++ parsePassport pr

splitToken :: String -> (String, String)
splitToken token = (take 3 token, drop 4 token)

checkPassportFields :: [(String, String)] -> Int
checkPassportFields [] = 0
checkPassportFields (f:fr)
    | fst f == "byr" && (checkByr $ snd f) = 1 + checkPassportFields fr
    | fst f == "iyr" && (checkIyr $ snd f) = 1 + checkPassportFields fr
    | fst f == "eyr" && (checkEyr $ snd f) = 1 + checkPassportFields fr
    | fst f == "hgt" && (checkHgt $ snd f) = 1 + checkPassportFields fr
    | fst f == "hcl" && (checkHcl $ snd f) = 1 + checkPassportFields fr
    | fst f == "ecl" && (checkEcl $ snd f) = 1 + checkPassportFields fr
    | fst f == "pid" && (checkPid $ snd f) = 1 + checkPassportFields fr
    | otherwise = checkPassportFields fr
    
checkByr :: String -> Bool
checkByr t = 1920 <= val && val <= 2002 
    where val = (read t :: Int)

checkIyr :: String -> Bool
checkIyr t = 2010 <= val && val <= 2020
    where val = (read t :: Int)

checkEyr :: String -> Bool
checkEyr t = 2020 <= val && val <= 2030
    where val = (read t :: Int)

checkHgt :: String -> Bool
checkHgt t
    | suffix == "in" && (59 <= number && number <= 76)      = True
    | suffix == "cm" && (150 <= number && number <= 193)    = True 
    | otherwise                                             = False
    where suffix = drop ((length t) - 2) t
          number = read (take ((length t) - 2) t) :: Int

checkHcl :: String -> Bool
checkHcl t
    | f == '#' && (length t == 7) = True
    | otherwise                   = False
    where f = head t

checkEcl :: String -> Bool
checkEcl t
    | elem t ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]  = True
    | otherwise                                                 = False

checkPid :: String -> Bool
checkPid t = (length t == 9) && (read t :: Int) > 0
