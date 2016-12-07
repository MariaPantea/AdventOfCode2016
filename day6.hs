import Data.List
import Data.Char
import Data.Ord

makeList :: [String] -> [String]
makeList [] = []
makeList as = if head as /= []  
              then map head as : makeList (map tail as)
              else []

toTuples :: String -> [(Char,Int)]
toTuples [] = []
toTuples (a:as) = (a,n) : toTuples as' 
  where 
    n = 1 + (length $ takeWhile (== a) as)
    as' = dropWhile (== a) as

main :: IO ()
main = do
  file <- readFile "input6.txt"
  --putStrLn $ show $ map (fst . (maximumBy (comparing snd)) . toTuples . sort) $ makeList $ lines file
  putStrLn $ show $ map (fst . (minimumBy (comparing snd)) . toTuples . sort) $ makeList $ lines file