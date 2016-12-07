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


-- Takes a string of length 3
isABA :: String -> Bool
isABA s = a /= b && a == reverse b 
  where 
    a = take 2 s
    b = drop 1 s


isValid' :: String -> [String]
isValid' "" = []
isValid' s = if isABA aba 
             then aba : (isValid' $ tail s)
             else isValid' $ tail s
  where 
    aba = take 3 s

everysecond :: [String] -> [String]
everysecond [] = []
everysecond (a:as) = a : as'
  where
    as' = if as == [] then [] else everysecond (tail as)

-- For one IP
isSSL :: [[String]] -> [[String]] -> [Bool]
isSSL [] _ = []
isSSL (a:as) (b:bs) = if a /= [] 
                      then apa a b : isSSL as bs
                      else False : isSSL as bs

apa :: [String] -> [String] -> Bool
apa [] _ = False
apa (a:as) bs = isSubsequenceOf [a] bs || apa as bs 


main :: IO ()
main = do
  file <- readFile "input7.txt"
  aba <- return $ map ((map init) . concat . (map isValid') . everysecond . rearrange) $ lines file
  bab <- return $ map ((map tail) . concat . (map isValid') . everysecond) $ map (tail . rearrange) $ lines file
  print $ length $ filter (==True) $ isSSL aba bab

  
