module Split where

splitComma :: String -> [String]
splitComma s = sp_c s ""


sp_c :: String -> String -> [String]
sp_c (c:rest) cum
    | c == ',' && cum /= ""  = [cum] ++ sp_c rest ""
    | otherwise = sp_c rest (cum ++ [c])
sp_c [] cum
    | cum == ","  = []
    | otherwise = [cum]
