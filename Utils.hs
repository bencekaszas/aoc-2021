module Utils where

import Data.Char
import Control.Applicative
import Control.Monad

data Command
  = Forward Int
  | Up Int
  | Down Int
  deriving (Show)

parseCommand :: String -> Command
parseCommand s =
  case words s of
    ["forward", x] -> Forward (read x)
    ["up", x] -> Up (read x)
    ["down", x] -> Down (read x)
    _ -> error $ "Invalid input: " ++ s

subMatrices :: Int -> [[Int]] -> [[[Int]]]
subMatrices size = concatMap subsFromRowGroup . rowGroups
  where
    rowGroups = chunksOf size
    subsFromRowGroup = map transpose . chunksOf size . transpose

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = ys : chunksOf n zs
  where
    (ys, zs) = splitAt n xs


main :: IO ()
main = do
  input <- map parseCommand . lines <$> readFile "inputs/02.txt"
  print(show(input))
