module Split where

splitCommai :: String -> [Int]
splitCommai s = sp_ci s ""

splitStriInt :: String -> [Int]
splitStriInt (c:rest) = [(read [c] :: Int)] ++ splitStriInt rest
splitStriInt [] = []

sp_ci :: String -> String -> [Int]
sp_ci (c:rest) cum
    | c == ','  = [(read cum :: Int)] ++ sp_ci rest ""
    | otherwise = sp_ci rest (cum ++ [c])
sp_ci [] cum
    | cum == ","  = [] -- Should be notNumber or equiv
    | otherwise = [read cum]

sp_ci_on :: String -> Char -> String -> [Int]
sp_ci_on (c:rest) on_c cum
    | c == on_c  = [(read cum :: Int)] ++ sp_ci_on rest on_c ""
    | otherwise = sp_ci_on rest on_c (cum ++ [c])
sp_ci_on [] on_c cum
    | cum == [on_c]  = [] -- Should be notNumber or equiv
    | otherwise = [read cum]


split :: String -> String -> [String]
split (c:rest) cum
    | c == ' '      = [cum] ++ split rest ""
    | otherwise     = split rest (cum ++ [c])
split [] cum
    | cum == " "    = []
    | otherwise     = [cum]

split_on :: String -> Char -> String -> [String]
split_on (c:rest) split_char cum
    | c == split_char      = [cum] ++ split_on rest split_char ""
    | otherwise            = split_on rest split_char (cum ++ [c])
split_on [] split_char cum
    | cum == [split_char]  = []
    | otherwise            = [cum]
