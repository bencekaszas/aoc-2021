module Day11 where

import Data.List.Split
import Data.Char
import Data.List

type Point = (Int,Int)
type Value = Int
type Grid = [(Point,Value)]



octoSim :: Grid -> Int -> Int -> Int
octoSim grid 0 count = count
octoSim grid n count = octoSim (nextStep grid [] []) (n-1) (count + (length $ filter (== 0) $ map snd (nextStep grid [] [])))

nextStep :: Grid -> [Point] -> [Point] -> Grid
nextStep grid queue done | or $ map isReady grid = nextStep [if isReady (p,v) then (p,0) else (p,v) | (p,v) <- grid]
                                                  ( filter (\x -> not $ x `elem` (done ++ map fst (filter isReady grid))) (concat (map (adjacents . fst) (filter isReady grid))))
                                                  (done ++ map fst (filter isReady grid))
                         | queue == [] = [if p `elem` done then (p,0) else (p,v+1) | (p,v) <- grid]
                         | otherwise = nextStep (foldl increase grid queue) [] done

allFlash :: Grid -> Int -> Int
allFlash grid n | (length $ filter (== 0) $ map snd (nextStep grid [] [])) == 100 = n
                | otherwise = allFlash (nextStep grid [] []) (n+1)

isReady :: (Point,Value) -> Bool
isReady (p,9) = True
isReady (p,x) = False

increase :: Grid -> Point -> Grid
increase grid point = [if p == point && v /= 9 then (p,v+1) else (p,v)| (p,v) <- grid]

adjacents :: Point -> [Point]
adjacents (x,y) = [ (x+m, y+n) | m <- [-1,0,1], n <- [-1,0,1], (x+m) /= -1, (y+n) /= -1, (x+m) /= 10, (y+n) /= 10, (m,n) /= (0,0)]

getGrid :: [[Int]] -> Grid
getGrid matrix = [((m,n), ((matrix !! n) !! m)) | n <- [0.. length matrix -1], m <- [0..length matrix -1] ]

main :: IO ()
main = do
  inps <- lines <$> readFile "inputs/11.txt"
  let xs = getGrid $ map (map digitToInt) inps :: Grid
      test = [(0,0),(1,0),(2,0)] :: [Point]
  print("Part 1: " ++ show(octoSim xs 100 0))
  print("Part 2: " ++ show(allFlash xs 1))
