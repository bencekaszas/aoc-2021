module Day06 where

import Data.List.Split (splitOn)

grow day xs =  sum ((iterate nextDay . dayCount $ xs) !! day)
  where
    dayCount xs = [length $ filter (== daysLeft) xs | daysLeft <- [0..8]]
    nextDay [zero,one,two,three,four,five,six,seven,eight] = [one, two, three, four, five, six, seven + zero, eight, zero]

main :: IO ()
main = do
    inps <- readFile "inputs/06.txt"
    let xs = map read $ splitOn "," inps :: [Int]
    print ("Part 1: " ++ show (grow 80 xs))
    print ("Part 2: " ++ show (grow 256 xs))
