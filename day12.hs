import Data.Map (Map)
import qualified Data.Map as Map

type State = (Int,Map Char Int)


parse :: Int -> [String] -> Map Char Int -> State
parse p ["cpy", x, y] regs  
  | elem (head x) ['a','b','c','d'] = (p+1, Map.insert (head y) (regs Map.! (head x)) regs)
  | otherwise                       = (p+1, Map.insert (head y) (read x) regs)

parse p ["inc", x] regs = (p+1, Map.adjust (+1) (head x) regs)
parse p ["dec", x] regs = (p+1, Map.adjust (\a -> a-1) (head x) regs)

parse p ["jnz", x, y] regs  
  | elem x' ['a','b','c','d'] = (jump p (regs Map.! x') y', regs)
  | otherwise                 = (jump p (read x)        y', regs)
    where
      x' = head x
      y' = read y 

jump :: Int -> Int -> Int -> Int
jump p x y 
  | x == 0      = p + 1
  | otherwise   = p + y


apa :: Int -> [[String]] -> State -> State
apa l insts (p, regs) 
  | p >= l = (p,regs)
  | otherwise = apa l insts $ parse p (insts !! p) regs


main :: IO ()
main = do 
  file <- readFile "input12.txt"
  inst <- return $ map words $ lines file

  -- Part 1
  (p,regs) <- return $ apa (length inst) inst 
          (0, Map.fromList [('a', 0), ('b', 0), ('c', 0), ('d', 0)])
  print $ regs Map.! 'a'

  -- Part 2
  (p,regs) <- return $ apa (length inst) inst 
          (0, Map.fromList [('a', 0), ('b', 0), ('c', 1), ('d', 0)])
  print $ regs Map.! 'a'