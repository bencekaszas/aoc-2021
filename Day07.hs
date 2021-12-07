module Day06 where

import Data.List.Split (splitOn)
import Data.List


median :: [Int] -> Int
median xs | odd $ length xs = (sort xs) !! mid
          | even $ length xs = (sort xs) !! (mid-1)
            where
              mid = length xs `div` 2

average :: [Int] -> Int
average xs = sum xs `div` length xs

disToAv :: [Int] -> Int -> Int
disToAv xs av =sum $ map toNew $ map dis xs
  where
    dis x = abs(x-av)
    toNew x = sum [ k | k <- [1..x]]

disToMed :: [Int] -> Int -> Int
disToMed xs med = sum $ map dis xs
  where
    dis x = abs(x-med)

main :: IO ()
main = do
    inps <- readFile "inputs/07.txt"
    let xs = map read $ splitOn "," inps :: [Int]
    print ("Part 1: " ++ show (disToMed xs (median xs)))
    print ("Part 2: " ++ show (disToAv xs (average xs)))
