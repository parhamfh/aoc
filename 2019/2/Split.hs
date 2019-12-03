module Split where

splitComma :: String -> [Int]
splitComma s = sp_c s ""


sp_c :: String -> String -> [Int]
sp_c (c:rest) cum
    | c == ','  = [(read cum :: Int)] ++ sp_c rest ""
    | otherwise = sp_c rest (cum ++ [c])
sp_c [] cum
    | cum == ","  = [] -- Should be notNumber or equiv
    | otherwise = [read cum]
