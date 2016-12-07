
-- 1 2 3
-- 4 5 6
-- 7 8 9

-- list = [1..9]

getNext :: Char -> (Int, Int) -> (Int, Int)
getNext next (x,y) = case next of
  'U' -> if y == 1 then (x,y) else (x, y+1) 
  'D' -> if y == -1 then (x,y) else (x, y-1)
  'R' -> if x == 1 then (x,y) else (x+1, y)
  'L' -> if x == -1 then (x,y) else (x-1, y)

coord :: (Int, Int) -> String -> (Int, Int)
coord (x,y) [] = (x,y)
coord (x,y) (a:as) = coord (getNext a (x,y)) as

getNumber :: (Int, Int) -> Int
getNumber (x,y) = case y of
  1    -> 1 + x+1 
  0    -> 4 + x+1
  (-1) -> 7 + x+1

main :: IO ()
main = do
  file <- readFile "input2.txt"
  putStrLn . show $ map (getNumber . (coord (0,0))) (lines file)
