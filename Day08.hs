module Day07 where

import Data.List.Split (splitOn)
import Data.List
import Control.Arrow

type Signal = ([String],[String])
type Map = [(String,String)]


decode :: Signal -> Map
decode (xs,ys) = sortOn snd (map whatNum xs)
  where
    whatNum x | length x == 2 = (x,"1")
              | length x == 3 = (x,"7")
              | length x == 4 = (x,"4")
              | length x == 7 = (x,"8")
              | length x == 5 = (x,"98")
              | length x == 6 = (x,"99")

decode' :: Map -> Map
decode' xs = sortOn snd $ map whatNum' xs
  where
    a = fst $ xs !! 0
    b = fst $ xs !! 1
    whatNum' (x,i) | i == "99" && (head a `elem` x) && (last a `elem` x) && (head fourMinOne `elem` x) && (last fourMinOne `elem` x) = (x,"9")
                   | i == "99" && (head a `elem` x) && (last a `elem` x) = (x,"0")
                   | i == "99" = (x,"6")
                   | i == "98" && (head a `elem` x) && (last a `elem` x) = (x,"3")
                   | i == "98" && (head fourMinOne `elem` x) && (last fourMinOne `elem` x) = (x, "5")
                   | i == "98" = (x,"2")
                   | otherwise = (x,i)
    fourMinOne = [x | x <- b, not (x `elem` a)]

findVal :: (Signal,Map) -> Int
findVal ((xs,ys),sol) = read $ concat [ snd s | y <- ys, s <- sol, y `elem` (permutations $ fst s)]

findSimples :: Signal -> Int
findSimples (xs,ys) = length [ y | y <- ys, length y == 2 || length y == 3 || length y == 4 || length y == 7]

tuplify :: [String] -> Signal
tuplify [x,y] = ( splitOn " " x,splitOn " " y)

main :: IO ()
main = do
    inps <- lines <$> readFile "inputs/08.txt"
    let xs = map tuplify $ map (splitOn " | ") inps
        maps = map decode' $ map decode xs
    print ("Part 1: " ++ show (sum $ map findSimples xs))
    print ("Part 2: " ++ show (sum $ map findVal (zip xs maps)))
