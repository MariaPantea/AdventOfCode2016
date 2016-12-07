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



getNext :: String -> (Dir, Int, Int) -> (Dir, Int, Int)
getNext s (d,x,y) = case newD of
  0 -> (newD, x,   y+n)
  1 -> (newD, x+n, y  )
  2 -> (newD, x,   y-n)
  3 -> (newD, x-n, y  )
  where 
    newD = getDir s d
    n = read (tail s) :: Int


coord :: [String] -> (Dir, Int, Int) -> (Dir, Int, Int)
coord [] c = c
coord (a:as) (d,x,y) = coord as (getNext a (d,x,y))

main :: IO ()
main = do
  file <- readFile "input.txt"
  putStrLn . show $ coord (splitOn (filter (/= ' ') file)) (0,0,0)
