import Data.List



parse :: String -> (Int,Int)
parse s = (read a, read $ tail b)
  where
    (a,b) = break (== '-') s


depa :: [(Int,Int)] -> [(Int,Int)]
depa [] = []
depa [x] = [x]
depa as = if a2 <= (b1+1) 
          then if b1 <= b2
               then depa $ (a1,b2):(drop 2 as)
               else depa $ (a1,b1):(drop 2 as)
          else (a1,b1) : depa (tail as)

  where
    (a1,b1) = as !! 0 
    (a2,b2) = as !! 1

epa :: [(Int,Int)] -> Int
epa [] = 0
epa ((a,b):as) = (b+1) - a + (epa as)


main :: IO ()
main = do 
  file <- readFile "input20.txt"
  as <- return $ sort $ map parse $ lines file
  print $ 1 + (snd . head $ depa as)
  print $ (4294967295+1) - (epa $ depa as)