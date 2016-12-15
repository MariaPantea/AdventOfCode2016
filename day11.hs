import Data.List
import qualified Data.Map as Map
import Data.Map (Map)


type Building = [[Int]] 

----- RULES -----

moveManyUp :: Building -> (Int,Int) -> (Maybe Building, Int)
moveManyUp b (i1,i2) 
  -- both microchips 
  | i1 < 10 && abs (i1 - i2) < 5 = case moveOneUp b i1 of
                                    (Just b',f) -> moveOneUp b' i2
                                    (Nothing,f) -> (Nothing,0)
  -- both generators
  | i1 + i2 > 20 = 
    if (((notElem (i2-10) onCurrentFloor) && (notElem (i1-10) onCurrentFloor)) || not (any (>10) (onCurrentFloor \\ [i1,i2])))
          && length (upperM \\ [i1-10,i2-10]) == 0 
    then okMovePair (i1,i2) b
    else (Nothing,0)

  -- one of each but pair
  | abs (i1 - i2) == 10 = if length upperM == 0
                          then okMovePair (i1,i2) b
                          else (Nothing,0)

  -- one of each but different
  | otherwise = (Nothing,0)

  where
        floor = case findIndex (==True) $ map (elem i1) b of
                Just f -> f
                Nothing -> error "The item is gone"
        onUpperFloor = b !! (floor+1) 
        (upperM,upperG) = aloneMsAndGs onUpperFloor
        onCurrentFloor = b !! floor
        (mSOnCurrent, currentG) = partition (<10) onCurrentFloor




moveManyDown :: Building -> (Int,Int) -> (Maybe Building,Int)
moveManyDown b i = case (moveManyUp (reverse b) i) of 
                    (Just b',f) -> (Just $ reverse b', case f of
                                                        1 -> 2
                                                        2 -> 1
                                                        3 -> 0
                                                        _ -> error "illegal floor" )
                    (Nothing,f) -> (Nothing, f)


moveOneUp :: Building -> Int -> (Maybe Building,Int)
moveOneUp b i = 
  if floor < 3 
  then 
    -- isMicrochip
    if i < 10
    then if (elem (i+10) upperG) || (length upperG) == 0 
         then (Just $ okMove i floor b, floor+1)
         else (Nothing,floor)
  
    -- isGenerator
    else      
      if length (upperM \\ [i-10]) == 0 && 
         (notElem (i-10) mSOnCurrent || not (any (>10) (onCurrentFloor \\ [i])))
      then (Just $ okMove i floor b, floor+1)
      else (Nothing,floor) 
  else (Nothing, floor)

      where
        floor = case findIndex (==True) $ map (elem i) b of
          Just f -> f
          Nothing -> error "The item is gone"
        onUpperFloor = b !! (floor+1) 
        (upperM,upperG) = aloneMsAndGs onUpperFloor
        onCurrentFloor = b !! floor
        (mSOnCurrent, currentG) = partition (<10) onCurrentFloor

-- floor starts at 0

moveOneDown :: Building -> Int -> (Maybe Building,Int)
moveOneDown b i = case moveOneUp (reverse b) i of
                  (Just b,f) -> (Just $ reverse b, case f of
                                                        1 -> 2
                                                        2 -> 1
                                                        3 -> 0
                                                        _ -> error "illegal floor" )
                  (Nothing,f) -> (Nothing,f)


-- Get alone microchips and all generators on a given floor
aloneMsAndGs :: [Int] -> ([Int],[Int])
aloneMsAndGs xs = (m \\ (map (\x -> x-10) g), g)
  where
    (m,g) = partition (<10) xs


----- MOVES -----

okMove :: Int -> Int -> Building -> Building
okMove i floor b = take (floor) b ++ [downstairs] ++ [upstairs] ++ drop (floor+2) b
  where
    upstairs = i : (b !! (floor+1))
    downstairs = delete i $ b !! (floor)


okMovePair :: (Int,Int) -> Building -> (Maybe Building, Int)
okMovePair (i1,i2) b = if floor < 3 
                       then (Just $ okMove i2 floor $ okMove i1 floor b, floor+1)
                       else (Nothing,0)
  where
    floor = case findIndex (==True) $ map (elem i1) b of
            Just f -> f
            Nothing -> error "The item is gone"

----- FIND POSSIBLE MOVES -----

createPairs :: [Int] -> [(Int,Int)]
createPairs [] = []
createPairs (a:as) = (zip (repeat a) as) ++ createPairs as


getTuple :: (Maybe Building,Int) -> (Building,Int)
getTuple (Just b, f) = (b,f)

findPossible :: (Building, Int) -> [(Building, Int)]
findPossible (b, floor) = 
 case floor of 
    0 -> removeDubbles uppers 
    3 -> removeDubbles downers
    _ -> removeDubbles $ uppers ++ downers

    where
      onCurrentFloor = b !! floor
      pairsOnCurrent = createPairs onCurrentFloor
      uppers = map getTuple $ filter ((/= Nothing).fst) $ (map (moveOneUp b) onCurrentFloor) 
                                   ++ (map (moveManyUp b) pairsOnCurrent) 
      downers = map getTuple $ filter ((/= Nothing).fst) $ (map (moveOneDown b) onCurrentFloor) 
                                   ++ (map (moveManyDown b) pairsOnCurrent) 


bepa :: Int -> [(Building, Int)] -> [(Building, Int)] -> Int
bepa 37 _ _ = 0
bepa n old bs = if any (==True) $ map isSolution bs
                then n
                else bepa (n+1) newOld newb
  where
    newOld = removeDubbles $ old ++ bs
    newb = removeDubbles $ concat $ map findPossible bs


isSolution :: (Building,Int) -> Bool
isSolution (b,i) = length (head b) == numOfItems

sortFloors :: [(Building,Int)] -> [(Building,Int)]
sortFloors [] = []
sortFloors ((b,i):bs) = (map sort b,i) : sortFloors bs

removeDubbles :: [(Building,Int)] -> [(Building,Int)]
removeDubbles bs = nub $ sortFloors bs


numOfItems = 10

main = do
  b <- return $ [[1,11],[12,13,14,15],[2,3,4,5],[]]
  c <- return $ [[1,2],[12],[11],[]]
  h <- return $ Map.fromList [(([(0,1),(0,2)],0), 'x')]
  print $ Map.insert ([(0,2),(0,1)],0) 'x' h
  --print $ bepa 1 [] $ findPossible (b,0)





















