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

getControl :: (String,String,Int) -> String
getControl (a,b,c) = b

decrypt :: (String, Int) -> String
decrypt ([],_) = []
decrypt ((a:as),n) = (list !! (i + n)) : decrypt (as,n)
  where
    i = head $ elemIndices a list
    list = cycle ['a'..'z']


main :: IO ()
main = do
  file <- readFile "input4.txt"
  files <- return $ lines file
  letters <- return $ map (takeWhile isLetter) (map (filter (/= '-')) files)
  digits <- return $ map (toDigit . filter isDigit) files
  decrypted <- return $ map decrypt $ zip letters digits
  putStrLn . show $ filter ((isInfixOf "northpole").fst) $ zip decrypted digits



