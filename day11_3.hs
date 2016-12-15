import Data.List



type Building = (Int,[(Int,Int)])

----- MOVES -----

isSafe :: (Int,Int) -> Bool
isSafe (a,b) = a == b

moveOneUp :: Building -> (Int,Int) -> Int -> Maybe Building
moveOneUp (f,b) (m,g) i
  -- Microchip
  | i == 1 = if g == f+1 || onUpperG == 0
              then Just (f+1, (m+1,g):(b \\ [(m,g)]))
              else Nothing

  -- Generator
  | i == 2 = if (m /= f || onCurrentG == 1) 
                && (onUpperUnsafeM == 0 || m == f+1 && onUpperUnsafeM == 1)
              then Just (f+1, (m,f+1):(b \\ [(m,g)]))
              else Nothing

  where
    onUpperUnsafeM = length $ filter (not . isSafe) onUpperM 
    onUpperM = filter ((== f+1).fst) b
    onUpperG = length $ filter ((== f+1).snd) b
    onCurrentG = length $ filter ((== f).snd) b


moveOneDown :: Building -> (Int,Int) -> Int -> Maybe Building
moveOneDown (f,b) (m,g) i
  -- Microchip
  | i == 1 = if g == f-1 || onLowerG == 0
             then Just (f-1, (f-1,g) : (b \\ [(m,g)]))
             else Nothing
  
  -- Generator
  | i == 2 = if (m /= f || onCurrentG == 1) && onLowerUnsafeM == 0
              then Just (f-1, (m,f-1) : (b \\ [(m,g)]))
              else Nothing

  where
    onLowerUnsafeM = length $ filter (not . isSafe) onLowerM 
    onLowerM = filter ((== f-1).fst) b
    onLowerG = length $ filter ((== f-1).snd) b
    onCurrentG = length $ filter ((== f).snd) b


moveManyUp :: Building -> (Int,Int) -> (Int,Int) -> Maybe Building
moveManyUp (f,b) (index1,i1) (index2,i2)
 
  -- both microchips
  | i1 + i2 == 2  = case moveOneUp (f,b) (m1,g1) i1 of 
                          Just (f',b') -> moveOneUp (f,b') (m2,g2) i2
                          Nothing -> Nothing

  -- both Generators
  | i1 + i2 == 4  = if (m1 /= f && m2 /= f || onCurrentG == 2) 
                        && 
                        (onUpperUnsafeM == 0 || (onUpperUnsafeM == 1 && (m1 == f+1 || m2 == f+1))
                          || (onUpperUnsafeM == 2 && m1 == f+1 &&m2 == f+1))
                    then Just (f+1,(m1,f+1) : (m2,f+1) : b')
                    else Nothing
  -- a pair
  | index1 == index2 = if onUpperUnsafeM == 0
                          then Just (f+1,(f+1,f+1):(b \\ [(m1,g1)]))
                          else Nothing 

    where
    onUpperUnsafeM = length $ filter (not . isSafe) onUpperM 
    onUpperM = filter ((== f+1).fst) b
    onCurrentG = length $ filter ((== f).snd) b
    (m1,g1) = b !! index1
    (m2,g2) = b !! index2
    b' = b \\ [(m1,g1),(m2,g2)]

     

moveManyDown :: Building -> (Int,Int) -> (Int,Int) -> Maybe Building
moveManyDown (f,b) (index1,i1) (index2,i2)

  -- both microchips
  | i1 + i2 == 2  = case moveOneDown (f,b) (m1,g1) i1 of 
                          Just (f',b') -> moveOneDown (f,b') (m2,g2) i2
                          Nothing -> Nothing

  -- both Generators
  | i1 + i2 == 4  = if (m1 /= f && m2 /= f || onCurrentG == 2) 
                        && 
                        (onLowerUnsafeM == 0 || (onLowerUnsafeM == 1 && (m1 == f-1 || m2 == f-1))
                          || (onLowerUnsafeM == 2 && m1 == f-1 && m2 == f-1))
                    then Just (f-1,(m1,f-1) : (m2,f-1) : b)
                    else Nothing
  -- a pair
  | index1 == index2 = if onLowerUnsafeM == 0
                          then Just (f-1,(f-1,f-1):(b \\ [(m1,g1)]))
                          else Nothing 

    where
    onLowerUnsafeM = length $ filter (not . isSafe) onLowerM 
    onLowerM = filter ((== f-1).fst) b
    onCurrentG = length $ filter ((== f).snd) b
    (m1,g1) = b !! index1
    (m2,g2) = b !! index2
    b'  = b \\ [(m1,g1),(m2,g2)]




----- FIND NEXT STATES ------

-- findPossible :: Building -> [Maybe Building] 
-- findPossible b = undefined

--   where
--     onCurrentG = filter ((== f).snd) b
--     onCurrentM = filter ((== f).fst) b

----- REMOVE VISITED STATES ------





main :: IO ()
main = do
  b <- return $ (0,[(0,0),(2,1),(2,1),(2,1),(2,1)])
  print b






