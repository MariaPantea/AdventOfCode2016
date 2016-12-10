import Data.Char


code :: String -> String
code [] = []
code s = if '(' `elem` s 
         then a ++ concat (replicate n2 (take n1 s')) ++ (code $ drop n1 s')
         else s
  where
    a = takeWhile (/= '(') s
    b = takeWhile (/= ')') $ dropWhile (/= '(') s
    n1 = read (takeWhile isDigit $ tail b) :: Int
    n2 = read (tail $ dropWhile (/= 'x') $ b) :: Int 
    s' = drop (length a + length b + 1) s

main :: IO ()
main = do 
  file <- readFile "input9.txt"
  print $ length $ code $ filter (/= ' ') file