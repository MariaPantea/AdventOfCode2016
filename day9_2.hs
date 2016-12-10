import Data.Char
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Foldable

--      (4 x 7 ) OUHB
--  seq1|n1 n2| seqTD | rest
parse :: Seq Char -> (Seq Char, Seq Char, Seq Char, Seq Char, Int)
parse seq = (seq1, rest1, seqToDubble, rest, n2)
  where
    (seq1,rest1)  = Seq.breakl (== '(') seq
    (nums, rest2) = Seq.breakl (== ')') rest1
    (n1, n2)  = parseNums nums
    (seqToDubble, rest) = Seq.splitAt n1 $ Seq.drop 1 rest2

parseNums :: Seq Char -> (Int,Int)
parseNums s = (read . toList $ a,read . toList $ Seq.drop 1 b)
    where (a,b) = Seq.breakl (=='x') . Seq.drop 1 $ s


getSequence :: Seq Char -> Seq Char 
getSequence seq
  | Seq.null rest1 = seq1
  | otherwise      = mconcat [seq1, 
                      mconcat . replicate n2 . getSequence $ seqToDubble, 
                      getSequence rest]
  where
    (seq1, rest1, seqToDubble, rest, n2) = parse seq


main = do
  input <- Seq.fromList . head . lines <$> readFile "input9.txt"
  putStrLn $ show $ Seq.length $ getSequence input
















-- code :: String -> String
-- code [] = []
-- code s = if '(' `elem` s 
--          then a ++ concat (replicate n2 (take n1 s')) ++ (code $ drop n1 s')
--          else s
--   where
--     a = takeWhile (/= '(') s
--     b = takeWhile (/= ')') $ dropWhile (/= '(') s
--     n1 = readInt $ takeWhile isDigit $ tail b
--     n2 = readInt $ tail $ dropWhile (/= 'x') $ b
--     s' = drop (length a + length b + 1) s

-- getLength :: String -> Int
-- getLength s = if '(' `elem` s 
--               then getLength $ code s
--               else length s

-- readInt :: String -> Int
-- readInt i = read i :: Int

-- getBlocks :: String -> [(String, Int)]
-- getBlocks [] = []
-- getBlocks s = if head s /= '('
--               then (cs, 1) : getBlocks (drop (length cs) s)
--               else (as, i) : getBlocks (drop (n+1) $ dropWhile (/= ')') s)
--   where
--     as = take n $ bs -- the block
--     bs = tail $ dropWhile (/= ')') s -- string after parenthesis
--     n  = readInt $ takeWhile isDigit $ tail s -- length of block
--     i  = readInt $ takeWhile isDigit $ tail $ dropWhile (/= 'x') s -- multiple of the block 
--     cs = takeWhile (/= '(') s

-- main :: IO ()
-- main = do 
--   file <- readFile "input9.txt"
--   blocks<- return $ getBlocks file 
--   print $ map (getLength.fst) blocks

