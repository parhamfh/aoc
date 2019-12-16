import System.IO
import Control.Monad
import Debug.Trace
import Split
import qualified Data.Map.Strict as Map

type Orbit = (String, String)

main = do
    handle <- readFile "input.txt"
    let orbits = lines handle
    print $ countOrbits (map (\x -> (x!!0,x!!1)) (map splitOrbit orbits))

countOrbits :: [Orbit] -> Int
countOrbits orbs = totalOrbits orbMap
    where orbMap = traverseOrbits orbs Map.empty

-- This was totally unneccesary since it specifies they can only orbit one... duh
traverseOrbits :: [Orbit] -> Map.Map String [String] -> Map.Map String [String]
traverseOrbits ((orbited, orbiter):rest) m = traverseOrbits rest (Map.insert orbiter (m_list ++ [orbited]) m)  
    where m_list = maybe [] id (Map.lookup orbiter m)
traverseOrbits [] m = m

totalOrbits :: Map.Map String [String] -> Int
totalOrbits m = sumOrbits nuuu
    where nuuu = Map.map (numOrbits m) m

-- last term unnecessary since only orbit one
numOrbits :: Map.Map String [String] -> [String] -> Int
numOrbits m (orbited:rest) = 1 + (numOrbits m rest) + (numOrbits m indirect_orbit) -- unnecessary last term
    where indirect_orbit = maybe [] id (Map.lookup orbited m)
numOrbits m [] = 0

sumOrbits :: Map.Map String Int -> Int
sumOrbits m = foldl (\sum (x,y) -> sum+y) 0 m_list
    where m_list = Map.toAscList m
