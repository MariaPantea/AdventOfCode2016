import Data.List

type Dir = Int

splitOn :: [Char] -> [[Char]]
splitOn [] = []
splitOn xs = takeWhile (/= ',') xs : splitOn rest
  where
    rest = if (dropWhile (/= ',') xs) == "" 
           then [] 
           else (tail (dropWhile (/= ',') xs))


-- North = 0
-- East = 1
-- South = 2
-- West = 3
getDir :: String -> Dir -> Dir
getDir (x:xs) oldDir = case x of
  'R' -> mod (oldDir+1) 4
  'L' -> mod (oldDir-1) 4



getNext :: String -> (Dir, Int, Int) -> [(Dir, Int, Int)]
getNext s (d,x,y) = case newD of
  0 -> [(newD,x,y+n') | n' <- [1 .. n]]
  1 -> [(newD,x+n',y) | n' <- [1 .. n]]
  2 -> [(newD,x,y-n') | n' <- [1 .. n]]
  3 -> [(newD,x-n',y) | n' <- [1 .. n]]
  where 
    newD = getDir s d
    n = read (tail s) :: Int


coord :: [String] -> (Dir, Int, Int) -> [(Dir, Int, Int)]
coord [] _ = []
coord (a:as) (d,x,y) = (getNext a (d,x,y)) ++ coord as (last (getNext a (d,x,y)))


createCoordList :: [(Int,Int,Int)] -> [(Int,Int)]
createCoordList xs = zip (map xCoord xs) (map yCoord xs)
  where
   xCoord (_,a,_) = a
   yCoord (_,_,b) = b


checkList :: [(Int,Int)] -> [(Int,Int)] -> (Int,Int)
checkList (x:xs) ys = if x == (0,0) 
                      then (0,0) 
                      else if x `elem` ys 
                           then x 
                           else checkList xs (x:ys)

main :: IO ()
main = do
  file <- readFile "input.txt"
  putStrLn . show $ checkList (createCoordList (coord (splitOn (filter (/= ' ') file)) (0,0,0))) []




