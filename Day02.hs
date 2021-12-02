module Day02 where

import System.IO
import Data.Char
import Data.List

pos :: [String] -> [Int]
pos xs = (map fst (dirTuples xs))

depth :: [String] -> [Int]
depth xs = (map snd (dirTuples xs))

dirTuples :: [String] -> [(Int, Int)]
dirTuples xs =(map evalDir (map getDir xs))

evalDir :: (String, Int) -> (Int, Int)
evalDir (s,i) | isInfixOf "for" s = (i, 0)
              | isInfixOf "up" s = (0,-i)
              | isInfixOf "down" s = (0,i)

getDir :: String -> (String, Int)
getDir xs = (init xs, digitToInt (last xs))

first :: [String] -> (Int,Int)
first xs = (sum $ pos xs, sum $ depth xs)
-----------------------------------------------------------

dirTuples' :: [String] -> [(Char, Int)]
dirTuples' xs =(map evalDir' (map getDir xs))

evalDir' :: (String, Int) -> (Char, Int)
evalDir' (s,i) | isInfixOf "for" s = ('f', i)
               | isInfixOf "up" s = ('u', i)
               | isInfixOf "down" s = ('d', i)


update :: ((Int,Int),Int) -> (Char, Int) -> ((Int, Int), Int)
update ((p, d), a) ('f', i) = ((p+i, d + a*i), a)
update ((p, d), a) ('u', i) = ((p, d), a - i)
update ((p, d), a) ('d', i) = ((p, d), a + i)

second :: [String] -> (Int, Int)
second xs = fst $ foldl update ((0,0), 0) (dirTuples' xs)

main :: IO ()
main = do
  inps <- readLines "inputs/02.txt"
  print ("#1: " ++ show (first inps))
  print ("#2: " ++ show (second inps))

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile
