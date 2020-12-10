import System.IO
import Control.Monad
import Debug.Trace

import Ham


main = do
    handle <- readFile "input.txt"
    let ruleStrings = lines handle
    let rules = parseRules ruleStrings

    print $ length $ containsBags (filter (\x -> name x == "shinygold") rules) rules []


data Rule = Rule { name :: String
                 , contents :: [Content]
                 } deriving (Show, Eq)
data Content = Content { quantity :: Int
                       , cname :: String
                       } deriving (Show, Eq)

--
--  PARSING
--

parseRules :: [String] -> [Rule]
parseRules []   = []
parseRules (r:rs) = tokenize r : (parseRules rs)

tokenize :: String -> Rule
tokenize s = parseRule $ split s ""

parseRule :: [String] -> Rule
parseRule s = Rule name $ itemContent $ drop (contain_index+1) s
    where contain_index = find "contain" s 0
          name = foldr1 (++) $ take (contain_index-1) s

itemContent :: [String] -> [Content]
itemContent [] = []
itemContent (i:is)
    | i == "no" = []
    | otherwise = Content num name : (itemContent $ drop 3 is)
    where num = read i :: Int
          name = foldr1 (++) (take 2 is)

--
--  Calculate
--

containsBags :: [Rule] -> [Rule] -> [Rule] -> [Rule]
containsBags [] rules acc = acc
containsBags (r:rs) rules acc = containsBags (rs++newContainers) rules (acc ++ newContainers)
    where   containers      = containerBags (name r) rules 
            newContainers   = filter (\x -> notElem x acc) containers

containerBags :: String -> [Rule] -> [Rule]
containerBags bagName rules = filter (hasBag bagName) rules

hasBag :: String -> Rule -> Bool
hasBag s r = length (filter (\x -> cname x == s) (contents r)) > 0
