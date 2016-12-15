import Data.Matrix
import Data.Char
-- display = 6 x 50
mi = 6
mj = 50

rect :: Int -> Int -> Matrix Int -> Matrix Int
rect j i m = (m1 <|> m2) <-> m3 
  where
    m1 = matrix i j (\(a,b) -> 1)
    m2 = submatrix 1 i (j+1) mj m
    m3 = submatrix (i+1) mi 1 mj m

rotate :: String -> Int -> Int -> Matrix Int -> Matrix Int
rotate "row" row n m = rotateRow row n m
rotate "column" col n m = transpose $ rotateRow col n $ transpose m

rotateRow :: Int -> Int -> Matrix Int -> Matrix Int
rotateRow row n m = fromLists (m1 ++ m2 ++ m3)
  where
    m1 = take row $ toLists m 
    m2 = [rotate' n $ head . drop row $ toLists m] 
    m3 = drop (row+1) $ toLists m 

-- Shifts a vector n steps
rotate' :: Int -> [Int] -> [Int]
rotate' n as = b ++ a
  where
    (a,b) = splitAt (l-n) as
    l = length as

parse :: Matrix Int -> [[String]] -> Matrix Int
parse m [] = m
parse m (a:as) 
  | head a == "rect"   = parse (rect a' b' m) as
  | head a == "rotate" = parse (rotate (a !! 1) row n m) as
    where
      a' = read (takeWhile (/= 'x') $ a !! 1) :: Int
      b' = read (tail $ dropWhile (/= 'x') $ a !! 1) :: Int
      row = read (tail $ dropWhile (/= '=') $ a !! 2) :: Int
      n = read (last a) :: Int

main :: IO ()
main = do
  file <- readFile "input8.txt"
  insts <- return $ map words $ lines file 
  mat <- return $ parse (zero mi mj) insts
  print mat
  print $ length $ filter (== 1) $ toList mat




