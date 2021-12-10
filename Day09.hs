module Day09 where

import Data.Char
import Data.List

findM :: [[Int]] -> [[((Int,Int),Int)]]
findM m = [(findAdj (m !! r) ((transpose m) !! c) r c) | r <- [1..((length m) -2)], c <- [1..((length m) - 2)]]
  where
    findAdj m tr r c | (m !! c) < (m !! (c-1)) && (m !! c) < (m !! (c+1)) && (tr !! r) < (tr !! (r-1)) && (tr !! r)  < (tr !! (r+1)) = [((r,c), (m !! c))]
                     | otherwise = []

bfs :: [[Int]] -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
bfs matrix [(r,c)] queue seen | ((matrix !! r) !! c) /= 9 && not((r,c) `elem` seen) = bfs matrix [(head $ adj (r,c))] ( queue ++ (tail $ adj (r,c))) (seen ++ [(r,c)])
                              | length queue == 0  = seen
                              | length queue == 1 = bfs matrix [(head $ queue)] [] seen
                              | otherwise = bfs matrix [(head $ queue)] (tail $ queue) seen
                                where
                                  adj (r,c) = [(r-1,c),(r+1,c),(r,c-1),(r,c+1)]

pad :: [[Int]] -> Int -> [[Int]]
pad xs size = [replicate size (digitToInt '9')] ++ xs ++ [replicate size (digitToInt '9')]

main :: IO ()
main = do
  inps <- lines <$> readFile "inputs/09.txt"
  let xs = map (map digitToInt) inps :: [[Int]]
      xs_p = pad xs (head $ map length xs)
      xs_pp = transpose (pad (transpose xs_p) (head $ map length (transpose xs_p)))
      lowpoints = map fst $ concat $ filter (not.null) $ findM xs_pp
      values = map snd $ concat $ filter (not.null) $ findM xs_pp
      basins = sort $ map length $ map (\x -> bfs xs_pp [x] [x] []) lowpoints
  print ("Part 1: " ++ show  (sum $ map (\x -> x + 1) values))
  print ("Part 2: " ++ show (product $ drop (length basins - 3) basins))
