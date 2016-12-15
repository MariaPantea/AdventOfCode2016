
import Data.List

-- Disc #6 has 7 positions; at time=0, it is at position 0.

parse :: [String] -> [Int]
parse s =  drop pos $ cycle [0..(nPos-1)]
  where
    --disc = read $ last $ s !! 1
    nPos = read $ s !! 3 
    pos  = read . init . last $ s



main :: IO ()
main = do
  file <- readFile "input10.txt"
  print $ lines file