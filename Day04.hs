module Day04 where

import System.IO
import Data.List (transpose)
import Data.List.Split (splitOn)

type Matrix = [[Int]]

solution :: [Matrix] -> [Int] -> Int -> String -> (Matrix,Int)
solution  ms s k mode | and (checkWinner ms (take k s)) && mode == "max" = (extractLoser ms (take k s), last (take k s))
                      | or (checkWinner ms (take k s)) && mode == "min" = (extractSol ms (take k s), last (take k s))
                      | otherwise = solution ms s (k+1) mode

totalScore :: (Matrix, Int) -> Int
totalScore (m,k) = sum[ sum (filter (/= 999) r) | r <- m] * k

extractLoser :: [Matrix] -> [Int] -> Matrix
extractLoser ms s = last [ (checkSol m s) | m <- ms, not $ isWinner (checkSol m (init s))]

extractSol :: [Matrix] -> [Int] -> Matrix
extractSol ms s = head [ (checkSol m s) | m <- ms, isWinner (checkSol m s)]

checkWinner :: [Matrix] -> [Int] -> [Bool]
checkWinner ms s = [ isWinner (checkSol m s) | m <- ms]

checkSol :: Matrix -> [Int] -> Matrix
checkSol m xs = [ [ if element `elem` xs then 999 else element | element <- row] | row <- m ]

isWinner :: Matrix -> Bool
isWinner m | ((replicate 5 999) `elem` m) || ((replicate 5 999) `elem` (transpose m)) = True
           | otherwise = False

stripFifth :: Matrix -> [Matrix]
stripFifth xs = [ drop (k*5 -5) (take (k*5) xs) | k <- [1..100]]

main :: IO ()
main = do
  input <- lines <$> readFile "inputs/04.txt"
  sol <- lines <$> readFile "inputs/04_sol.txt"
  let calls = map read $ splitOn "," (head sol) :: [Int]
      boards =stripFifth $ filter (not . null) [map read x | x <- (map words input)] :: [Matrix]
      winner = solution boards calls 1 "min"
      loser = solution boards calls 1 "max"
  print ("Part 1: " ++ show (totalScore winner))
  print ("Part 2: " ++ show (totalScore loser))
