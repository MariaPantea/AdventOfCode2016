import Data.List
import Data.List.Split



createNewSeq :: String -> String
createNewSeq a  
  | length a >= 35651584 = a
  | otherwise = createNewSeq $ a ++ ('0':b)
  where
    b = map flipBits $ reverse a
    flipBits '0' = '1'
    flipBits '1' = '0'


getCheckSum :: [String] -> String
getCheckSum [] = []
getCheckSum (a:as) 
  | isPair a  = ('1' : getCheckSum as) 
  | otherwise = ('0' : getCheckSum as)
    where
      isPair (a:b:bs) = a == b

validCheckSum :: String -> String
validCheckSum s 
  | odd $ length s = s
  | otherwise      = validCheckSum . getCheckSum $ chunksOf 2 s


main :: IO ()
main = do
  disc  <- return $ chunksOf 2 . take 35651584 $ createNewSeq "11110010111001001"
  print $ validCheckSum . getCheckSum $ disc