{-# LANGUAGE OverloadedStrings #-}

import Prelude as P
import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.List


isCode :: String -> Bool
isCode s = P.take 5 s == "00000" && digitToInt (s !! 5) < 8


getCode :: [Int] -> [String] -> [(Int, Char)]
getCode _ [] = []
getCode xs (a:as) = if (not (P.elem c xs)) 
                    then (c , (a !! 6)) : getCode (c:xs) as 
                    else getCode xs as
  where
    c = digitToInt (a !! 5)

makeCode :: [(Int,Char)] -> String
makeCode as = P.map snd $ sort as

main = do
  l <- return $ P.take 8 $ getCode [] $ P.filter isCode $ P.map (show . md5) $ P.map pack $ P.zipWith (++) (P.repeat "reyedfim") (P.map show [0..100000000])
  print $ makeCode l
  