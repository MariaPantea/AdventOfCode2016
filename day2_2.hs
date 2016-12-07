
--     1
--   2 3 4 
-- 5 6 7 8 9
--   A B C
--     D

getNext (x,y) c  = case c of
  'U' -> if x == y || x+y == 4 then (x,y) else (x,y+1)
  'D' -> if x == (-y) || x+(-y) == 4 then (x,y) else (x,y-1)
  'R' -> if x + (abs (y)) == 4 then (x,y) else (x+1,y)
  'L' -> if (abs y) - x == 0 then (x,y) else (x-1,y) 

coord :: (Int, Int) -> String -> (Int, Int)
coord (x,y) []     = (x,y)
coord (x,y) (a:as) = coord (getNext (x,y) a) as

getNumber :: (Int, Int) -> Int
getNumber (x,y) = case abs y of 
    0 -> (x + 5) 
    1 -> (x + 5) + (y * (-4))
    2 -> (x + 5) + (y * (-3))

main :: IO ()
main = do
  file <- readFile "input2.txt"
  putStrLn . show $ map (getNumber . coord (0,0)) (lines file)
