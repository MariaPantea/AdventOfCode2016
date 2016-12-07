
isTriangle :: (Int,Int,Int) -> Bool
isTriangle (a,b,c) =  a + b > c && b + c > a && c + a > b 


parseToTripple :: [String] -> (Int, Int, Int)
parseToTripple (a:b:c:cs) = (a', b', c')
  where
    a' = read a :: Int
    b' = read b :: Int
    c' = read c :: Int

main :: IO ()
main = do
  file <- readFile "input3_2.txt"
  putStrLn . show $ length $ filter isTriangle $ map (parseToTripple . words) (lines file)

