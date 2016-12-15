{-# LANGUAGE OverloadedStrings #-}


import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Hash.MD5


getHash :: String -> String
getHash s = md5s $ Str s

haveN :: Int -> String -> Char
haveN n s = if c /= []
              then head c
              else 'o'
  where
    c = concat $ filter ((>= n).length) $ group s

have5 :: String -> Bool
have5 s = c /= []
  where
    c = filter ((>= 5).length) $ group s 

isKey :: String -> [(Int,Char)] -> Int -> Bool 
isKey s as i = 
  case a of 
    'o' -> False
    _ -> elem a $ map snd as
  where
    a = haveN 3 $ getHash (s ++ show i)


getLastKey :: String -> Int -> Int -> [(Int,Char)] -> Int
getLastKey _ i 64 _ = i - 1
getLastKey s i n as = if isKey s as' i
                      then getLastKey s (i+1) (n+1) as'
                      else getLastKey s (i+1) n as'
  where 
    a = haveN 5 $ getHash (s ++ show (i+1000))
    as' = case a of 
      'o' -> filter ((> i).fst) as
      _ -> ((i+1000,a):as) 

getInit :: String -> Int -> [(Int,Char)] -> [(Int,Char)]
getInit s 1001 l = l
getInit s   i  l = case a of 
  'o' -> getInit s (i+1) l
  _ -> getInit s (i+1) ((i,a):l)

  where
    a = haveN 5 $ getHash (s ++ show i)


-- Part 2 - superslow because of the rehashes.. 

reHash :: String -> String
reHash s = iterate (md5s . Str)  s !! 2017

isKey' :: String -> [(Int,Char)] -> Int -> Bool 
isKey' s as i = 
  case a of 
    'o' -> False
    _ -> elem a $ map snd as
  where
    a = haveN 3 $ reHash (s ++ show i)

getLastKey' :: String -> Int -> Int -> [(Int,Char)] -> Int
getLastKey' _ i 64 _ = i - 1
getLastKey' s i n as = if isKey' s as' i
                      then getLastKey' s (i+1) (n+1) as'
                      else getLastKey' s (i+1) n as'
  where 
    a = haveN 5 $ reHash (s ++ show (i+1000))
    as' = case a of 
      'o' -> filter ((> i).fst) as
      _ -> ((i+1000,a):as) 

initialize :: [String] -> [(Int,Char)]
initialize as = filter ((/= 'o').snd) $ zip [0..1000] $ map ((haveN 5).reHash) as


main :: IO ()
main = do
  s <- return "jlmsuwbz"
  l <- return $ getInit s 1 []
  l' <- return $ initialize $ zipWith (++) (repeat s) (map show [0..1000])
  print $ getLastKey s 0 0 l
  print $ getLastKey' s 0 0 l'








