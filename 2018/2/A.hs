import Debug.Trace

main = do
    handle <- readFile "input.txt"
    let ids = lines handle
    print $ checksum ids

checksum :: [String] -> Int
checksum ids = (count 2 ids) * (count 3 ids)

count :: Int -> [String] -> Int
count number [id]       = occurences number id id
count number (id:ids)   = occurences number id id + count number ids  

occurences :: Int -> String -> String -> Int
occurences number [s] full_str  = 0
occurences number (s:str) full_str
    | number == length (filter (==s) full_str)  = 1
    | otherwise                                 = occurences number str full_str
