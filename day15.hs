
import Data.List

parse :: [String] -> [Int]
parse s = drop pos $ cycle [0..(nPos-1)]
  where
    nPos = read (s !! 3) :: Int 
    pos  = read $ init . last $ s :: Int

getDiscs :: [[String]] -> [[Int]]
getDiscs s = map parse s


start :: [[Int]] -> [[Int]] -> Int -> Int -> Int
start [] _ t1 t2 = t2
start (w:ws) as t1 t2 = if w !! 1 == 0
                    then start (map tail ws) as (t1+1) t2
                    else start (map tail as) (map tail as) (t2+1) (t2+1)

main :: IO ()
main = do
  file <- readFile "input15.txt"
  s <- return $ map words $ lines $ file 
  discs <- return $ getDiscs s
  print $ start discs discs 0 0