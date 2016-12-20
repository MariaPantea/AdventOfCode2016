import Data.Hash.MD5

type Location = (String,(Int,Int))


isLegal :: (Int,Int) -> Bool
isLegal (x,y) = x > 0 && x < 5 && y > 0 && y < 5 

isVault :: Location -> Bool
isVault (s,(4,4)) = True
isVault _         = False

getCode :: String -> String
getCode s = take 4 $ md5s $ Str s

isOpen :: Char -> Bool
isOpen c = elem c ['b','c','d','e','f']

getLocation :: Maybe Location -> Location
getLocation (Just l) = l




getNexts :: Location -> [Location]
getNexts l = map getLocation nexts
  where
    nexts = filter (/= Nothing) $ map (getNext l) $ filter (isOpen.snd) $ zip "UDLR" $ getCode $ fst l

getNext :: Location -> (Char,Char) -> Maybe Location
getNext (s,(x,y)) ('U', c) = 
  if isLegal (x,y-1)
  then Just (s ++ "U",(x,y-1))
  else Nothing

getNext (s,(x,y)) ('D', c) = 
  if isLegal (x,y+1)
  then Just (s ++ "D",(x,y+1))
  else Nothing

getNext (s,(x,y)) ('L', c) = 
  if isLegal (x-1,y)
  then Just (s ++ "L",(x-1,y))
  else Nothing

getNext (s,(x,y)) ('R', c) = 
  if isLegal (x+1,y)
  then Just (s ++ "R",(x+1,y))
  else Nothing


part1 :: [Location] -> String
part1 [] = error "No path found"
part1 (l:ls) = if isVault l
             then drop (length "hhhxzeay") $ fst l
             else part1 (ls ++ getNexts l)



part2 :: [Location] -> [Location] -> Int
part2 [] bs     = (maximum $ map (length.fst) bs) - length "hhhxzeay"
part2 (l:ls) bs = if isVault l
                  then part2 ls (l:bs)
                  else part2 (ls ++ getNexts l) bs





main :: IO()
main = do
  print $ part1 [("hhhxzeay",(1,1))]
  print $ part2 [("hhhxzeay",(1,1))] []


