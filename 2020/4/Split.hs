module Split where

splitCommai :: String -> [Int]
splitCommai s = sp_ci s ""

splitStri :: String -> [Int]
splitStri (c:rest) = [(read [c] :: Int)] ++ splitStri rest
splitStri [] = []

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
