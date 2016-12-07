import Data.Char
import Data.List
import Data.Ord

toTuples :: String -> [(Char,Int)]
toTuples [] = []
toTuples (a:as) = (a,n) : toTuples as' 
  where 
    n = 1 + (length $ takeWhile (== a) as)
    as' = dropWhile (== a) as

makeSeq :: [(Char,Int)] -> String
makeSeq [] = []
makeSeq xs = x' : makeSeq xs'
  where
    x' = fst $ maximumBy (comparing snd) (xs)
    xs' = [c | c <- xs, fst c /= x']

isCorrect :: (String, String, Int) -> Bool
isCorrect (a,b,c) = a == b

toDigit :: String -> Int
toDigit s = read s :: Int

getControl :: (String,String,Int) -> Int
getControl (a,b,c) = c


main :: IO ()
main = do
  file <- readFile "input4.txt"
  files <- return $ lines file
  letters <- return $ map ((take 5) . makeSeq . toTuples . reverse . sort) $ map (takeWhile isLetter) (map (filter (/= '-')) files)
  digits <- return $ map (toDigit . filter isDigit) files
  control <- return $ map (init . tail) $ map (dropWhile (/= '[')) files
  putStrLn . show $ sum $ map getControl $ filter isCorrect $ zip3 letters control digits