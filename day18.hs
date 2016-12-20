

isTrap :: String -> Bool
isTrap s  
  | head s == '0' = s!!2 == '^'
  | length s == 2 = s!!1 == '^'
  | otherwise = s!!0 /= s!!2

nextRow :: String -> String
nextRow [] = []
nextRow s 
  
  | length s == 2 =
    if isTrap $ (take 2 s) ++ "."
    then "^"
    else "."

  | otherwise = if isTrap (take 3 s)
    then ('^':nextRow (drop 1 s))
    else ('.':nextRow (drop 1 s))
  
apa :: Int -> String -> Int -> Int
apa 1 s bs = bs
apa n  s bs = apa (n-1) s' bs'
  where
    s' = nextRow ('0':s)
    bs' = bs + (length $ filter (== '.') s')

main :: IO()
main = do
  input <- return "^^.^..^.....^..^..^^...^^.^....^^^.^.^^....^.^^^...^^^^.^^^^.^..^^^^.^^.^.^.^.^.^^...^^..^^^..^.^^^^"
  bs <- return $ length $ filter (== '.') input
  print $ apa 400000 input bs
