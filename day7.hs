import Data.List


-- empty list in the end
rearrange :: String -> [String]
rearrange [] = []
rearrange s = a : b : rearrange (drop n s)
  where 
    a = takeWhile (/= '[') s
    b' = takeWhile (/= ']') (dropWhile (/= '[') s)
    b = if length b' > 2 then tail b' else []
    n = length a + length b + 2

-- Takes a string of length 4
isABBA :: String -> Bool 
isABBA s = a /= b && a == reverse b
  where 
    a = take 2 s
    b = drop 2 s

isValid :: String -> Bool
isValid "" = False
isValid s = isABBA (take 4 s) || (isValid $ tail s) 


everysecond :: [String] -> [String]
everysecond [] = []
everysecond (a:as) = a : as'
  where
    as' = if as == [] then [] else everysecond (tail as)

isTLS :: [String] -> Bool
isTLS xs = length (filter isValid (everysecond xs)) > 0 &&
  length (filter isValid (everysecond (tail xs))) == 0


main :: IO ()
main = do
  file <- readFile "input7.txt"
  print $ length $ filter isTLS $ map rearrange $ lines file


  
