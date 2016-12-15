--import Data.Matrix
import Data.Char
import Numeric (showIntAtBase)
import Data.Map (Map)
import qualified Data.Map as Map

type Coord = (Int,Int)


getNumber :: Int -> Int -> String
getNumber x y = showIntAtBase 2 intToDigit n ""
  where 
    n = x * x + 3 * x + 2 * x * y + y + y * y + 1350

isOpen :: Coord -> Bool
isOpen (x,y) = even $ length $ filter (== '1') $ getNumber x y

step :: Coord -> [Coord]
step (x,y) = filter isOpen valid
  where
  valid = filter ((\(a,b) -> a >= 0 && b >= 0)) [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

isNotVisited :: Map Coord Bool -> Coord -> Bool
isNotVisited v (x,y) = not (Map.member (x,y) v)

insertCoords :: [Coord] -> Map Coord Bool -> Map Coord Bool
insertCoords [] v = v
insertCoords (c:cs) v = insertCoords cs $ Map.insert c True v

apa :: [Coord] -> Int -> Map Coord Bool -> Int
apa coords n v 
  | elem (31,39) coords = n
  | otherwise           = apa nexts (n+1) v'

    where
      nexts = filter (isNotVisited v) $ concat $ map step coords
      v' = insertCoords coords v 
 

-- Part 2 
bepa :: [Coord] -> Int -> Map Coord Bool -> Map Coord Bool
bepa coords 51 v = v 
bepa coords n v = bepa nexts (n+1) v'
  where
    nexts = filter (isNotVisited v) $ concat $ map step coords
    v' = insertCoords coords v 



main :: IO ()
main = do
  print $ apa [(1,1)] 0 Map.empty 
  print $ Map.size $ bepa [(1,1)] 0 Map.empty



