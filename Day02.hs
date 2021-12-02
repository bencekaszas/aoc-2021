module Day02 where

import System.IO
import Data.Char
import Data.List

first :: [String] -> (Int,Int)
first xs = (sum $ map fst $ map evalDir $ map getDir xs, sum $ map snd $ map evalDir $ map getDir xs)

evalDir :: (String, Int) -> (Int, Int)
evalDir (s,i) | isInfixOf "forward" s = (i, 0)
              | isInfixOf "up" s = (0,-i)
              | isInfixOf "down" s = (0,i)

getDir :: String -> (String, Int)
getDir xs = (init xs, digitToInt (last xs))

second :: [String] -> (Int, Int)
second xs = fst $ foldl update ((0,0), 0) (map evalDir' (map getDir xs))

evalDir' :: (String, Int) -> (Char, Int)
evalDir' (s,i) | isInfixOf "forward" s = ('f', i)
               | isInfixOf "up" s = ('u', i)
               | isInfixOf "down" s = ('d', i)

update :: ((Int,Int),Int) -> (Char, Int) -> ((Int, Int), Int)
update ((p, d), a) ('f', i) = ((p+i, d + a*i), a)
update ((p, d), a) ('u', i) = ((p, d), a - i)
update ((p, d), a) ('d', i) = ((p, d), a + i)

main :: IO ()
main = do
  inps <- fmap lines $ readFile "inputs/02.txt"
  let f = first inps
      s = second inps
  print ("Part 1: " ++ show (f) ++ " with sum " ++ show(fst f * snd f))
  print ("Part 2: " ++ show (f) ++ " with sum " ++ show(fst s * snd s))
