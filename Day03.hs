module Day03 where

import Data.List
import Data.Ord
import Data.Char

gamma_rate_r :: [String] -> String
gamma_rate_r = map mostCommon .transpose

epsilon_rate_r :: [String] -> String
epsilon_rate_r = flipBin . map mostCommon . transpose

mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

flipBin :: String -> String
flipBin xs = map flip' xs
  where
    flip' '0' = '1'
    flip' '1' = '0'
    flip' x = x

mostAlike :: [String] -> String -> Int -> ([String] -> String) -> String
mostAlike ss str k f | length(mAlike ss str k) > 1 && k < length str = mostAlike (mAlike ss str k) (f (mAlike ss str k)) (k+1) f
                     | length(mAlike ss str k) == 1 = head (mAlike ss str k)
                     where
                       mAlike :: [String] -> String -> Int -> [String]
                       mAlike xs s k = [x | x <- xs,s !! k == x !! k]

main :: IO ()
main = do
  input <- fmap lines $ readFile "inputs/03.txt"
  let e = toDec $ epsilon_rate_r input
      g = toDec $ gamma_rate_r input
      g_raw = gamma_rate_r input
      e_raw = epsilon_rate_r input
      ox_gen = toDec $ mostAlike input g_raw 0 gamma_rate_r
      c02_gen = toDec $ mostAlike input e_raw 0 epsilon_rate_r
  print ("Part 1: " ++ show (e*g))
  print ("Part 2: " ++ show (ox_gen * c02_gen))
