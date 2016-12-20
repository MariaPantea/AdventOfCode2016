

apa :: [(Int,Int)] -> Int
apa ts
  | length ts == 1 = fst . head $ ts
  | last ts' == i  = apa tsOdd
  | otherwise      = apa tsEven
  where 
    (i,l) = last ts
    ts'   = map fst $ filter (odd.snd) ts
    tsOdd  = zip (i: init ts') [1..]
    tsEven = zip ts' [1..]

-- Part 2

bepa :: [Int] -> Int
bepa [x] = x
bepa as  = bepa . take i . tail . cycle $ as'
  where
    n = div (length as) 2 
    (a,b) = splitAt n as
    as' = (a ++ tail b)
    i = length as'


bepa2 :: [Int] -> Int
bepa2 as 
  | length as == 2 = head as
  | length as == 3 = last as
  | otherwise      = bepa2 $ (drop l a) ++ bs' ++ (take l a)
  where
    n = div (length as) 2 
    (a,(b:bs)) = splitAt n as
    bs' = if mod (length as) 2 /= 0 
         then remove' bs
         else remove' $ tail bs
    l = length bs - length bs' + 1


remove' :: [Int] -> [Int]
remove' [] = []
remove' (a:as) = a : remove' (drop 2 as)


main :: IO ()
main = do
  --print $ apa $ zip [1..3005290] [1..]
  print $ bepa2 [1..3005290]


