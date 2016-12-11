import Data.List
import Data.Char
import qualified Data.Map as Map
import Data.Map (Map)


type Id = Int
type Value = Int
type State = (Map Id [Value], Map Id (Id,Id))

-- Initialize Maps 
initialize :: [[String]] -> [(Id,[Value])] -> [(Id,[Value])]
initialize [] bs = bs
initialize (w:as) bs = if l == []
                       then initialize as $ (id,[value]):bs 
                       else initialize as $ (combine (id,[value]) (head l)) :(filter ((/= id) . fst) bs)
  where
    (id,value) = (read $ last w, read $ w !! 1) 
    l = filter ((==id) . fst) bs
    combine (i1,v1) (i2,v2) = (i1, sort $ v1++v2)

getInstructions :: [[String]] -> [(Id,(Id,Id))]
getInstructions [] = []
getInstructions (["bot", id, "gives", "low", "to", "bot", botL, "and", "high", "to", "bot", botH]:bs) = 
  (read id, (read botL,read botH)):getInstructions bs
getInstructions (["bot", id, "gives", "low", "to", "output", outL, "and", "high", "to", "bot", botH]:bs) =
  (read id, ((-1)*(read outL) - 1, read botH)):getInstructions bs
getInstructions (["bot", id, "gives", "low", "to", "output", outL, "and", "high", "to", "output", outH]:bs) = 
  (read id, ((-1)*(read outL) - 1, (-1)*(read outH) - 1)):getInstructions bs



findInst :: State -> (Id,[Value])
findInst (h1,inst) = case Map.lookup id h1 of
  Just v -> if length v == 2
            then (id,v)
            else findInst (h1, Map.delete id inst)
  Nothing -> findInst (h1, Map.delete id inst)
  where
    id = head $ Map.keys inst


apa :: State -> State
apa (h1,inst) = if Map.size inst > 0 
                then apa (insertValue (id1,v1) $ insertValue (id2,v2) h1, Map.delete id inst) 
                else (h1,inst)
  where
    (id,vs) = findInst (h1,inst)
    v1 = minimum vs
    v2 = maximum vs 
    (id1,id2) = inst Map.! id
    
insertValue :: (Id,Value) -> Map Id [Value] -> Map Id [Value]
insertValue (id,v) h = case Map.lookup id h of
  Just vs -> Map.insert id (v:vs) h
  Nothing -> Map.insert id [v] h


main = do
  file <- readFile "input10.txt"
  (inits,rest) <- return $ partition (elem "value") $ map words $ lines file
  hash <- return $ Map.fromList $ initialize inits []
  instructions <- return $ Map.fromList $ getInstructions rest
  state <- return (hash,instructions)
  h <- return $ fst $ apa state
  print $ Map.filter (`elem` [[61,17],[17,61]]) $ h
  -- Part 2
  print $ foldl (*) 1 $ concat [(h Map.! (-1)), (h Map.! (-2)), (h Map.! (-3))]












