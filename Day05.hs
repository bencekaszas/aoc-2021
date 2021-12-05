module Day05 where

import Data.List.Split (splitOn)
import Data.List (sortOn, groupBy)


type LineDef = ((Int,Int),(Int,Int))
type Line = [(Int, Int)]


toLinePart1 :: LineDef -> Line
toLinePart1 ((x1,y1),(x2,y2)) | x1 /= x2 && y1 /= y2 = []
                         | x2 == x1 && y2 > y1 = [(x1,y1 + k) | k <- [0..(y2-y1)]]
                         | x2 == x1 && y2 < y1 = [(x1, y2 + k) | k <- [0..(y1-y2)]]
                         | y2 == y1 && x2 > x1 = [(x1 + k, y1) | k <- [0..(x2-x1)]]
                         | y2 == y1 && x2 < x1 = [(x2 + k, y1) | k <- [0..(x1-x2)]]

toLinePart2 :: LineDef -> Line
toLinePart2 ((x1,y1),(x2,y2)) | x1 < x2 && y1 < y2 = [(x1 + k, y1 + k) | k <- [0..(x2-x1)]]
                              | x1 < x2 && y1 > y2 = [(x1 + k, y1 - k) | k <- [0..(y1-y2)]]
                              | x1 > x2 && y1 < y2 = [(x1 - k, y1 + k) | k <- [0..(x1-x2)]]
                              | x1 > x2 && y1 > y2 = [(x1 - k, y1 - k) | k <- [0..(x1-x2)]]
                              | otherwise = []

tuplify :: [String] -> LineDef
tuplify [x,y] = ((read $ head(splitOn "," x),read $ last(splitOn "," x)),(read $ head(splitOn "," y),read $ last(splitOn "," y)))

main :: IO ()
main = do
    inps <- fmap lines $ readFile "inputs/05.txt"
    let linedefs = map tuplify $ map (splitOn " -> ") inps :: [LineDef]
        p1Points = length $ filter (\x -> length x > 1) $ groupBy (==) $ sortOn fst $ sortOn snd  $ concat $ filter (/= []) $ map toLinePart1 linedefs
        p2Points = length $ filter (\x -> length x > 1) $ groupBy (==) $ sortOn fst $ sortOn snd  $ concat $ filter (/= []) $ (map toLinePart2 linedefs) ++ (map toLinePart1 linedefs)
    print ("Part 1: " ++ show (p1Points))
    print ("Part 2: " ++ show (p2Points))
