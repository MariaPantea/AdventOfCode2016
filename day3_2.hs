
isTriangle :: (Int,Int,Int) -> Bool
isTriangle (a,b,c) =  a + b > c && b + c > a && c + a > b 

parseList :: [Int] -> [Int]
parseList [] = []
parseList [a] = [a]
parseList (a:as) = a : parseList (drop 2 as)

makeList :: [Int] -> [[Int]]
makeList s = [parseList s, parseList (tail s), parseList (tail (tail s))]

makeInt :: String -> Int
makeInt s = read s :: Int

makeTriples :: [Int] -> [[Int]]
makeTriples [] = []
makeTriples (as) = [take 3 as] ++ makeTriples (drop 3 as)

maketriple :: [Int] -> (Int,Int,Int)
maketriple (a:b:c:cs) = (a,b,c)

main :: IO ()
main = do
  file <- readFile "input3.txt"
  putStrLn . show $ length $ filter isTriangle $ map maketriple $ 
  	concat $ map makeTriples $ makeList $ map makeInt (words file) 

