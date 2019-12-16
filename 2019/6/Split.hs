module Split where

splitOrbit :: String -> [String]
splitOrbit s = sp_char s ')' ""

sp_char :: String -> Char -> String -> [String]
sp_char (c:rest) char cum
    | c == char     = [cum] ++ sp_char rest char ""
    | otherwise     = sp_char rest char (cum ++ [c])
sp_char [] char cum
    | cum == (show char)    = [] -- Should be notNumber or equiv
    | otherwise             = [cum]

splitComma :: String -> [Int]
splitComma s = sp_c s ""

sp_c :: String -> String -> [Int]
sp_c (c:rest) cum
    | c == ','  = [(read cum :: Int)] ++ sp_c rest ""
    | otherwise = sp_c rest (cum ++ [c])
sp_c [] cum
    | cum == ","  = [] -- Should be notNumber or equiv
    | otherwise = [read cum]

splitStr :: String -> [Int]
splitStr (c:rest) = [(read [c] :: Int)] ++ splitStr rest
splitStr [] = []

