module Day10 where

import Data.List.Split (splitOn)
import Data.List
import Day07 (median)


corrupt :: String -> String -> String
corrupt [] expected = expected
corrupt ('(' : cs) expected = corrupt cs (')':expected)
corrupt ('[' : cs) expected = corrupt cs (']':expected)
corrupt ('{' : cs) expected = corrupt cs ('}':expected)
corrupt ('<' : cs) expected = corrupt cs ('>':expected)
corrupt (c : cs) expected | head expected == c = corrupt cs (tail expected)
                          | otherwise = [c]


scoring :: String -> Int -> Int
scoring [] score = score
scoring  (')' : cs) score = scoring cs (score * 5 + 1)
scoring  (']' : cs) score = scoring cs (score * 5 + 2)
scoring  ('}' : cs) score = scoring cs (score * 5 + 3)
scoring  ('>' : cs) score = scoring cs (score * 5 + 4)

main :: IO ()
main = do
    inps <- lines <$> readFile "inputs/10.txt"
    let corrupts = group $ sort $ filter (\x -> length x == 1) $ map (\x -> corrupt x []) inps
        incompletes = filter (\x -> length x /= 1) $ map (\x -> corrupt x []) inps
    print ("Part 1: " ++ show (length (corrupts !! 0) * 3 + length(corrupts !! 1) * 25137 + length (corrupts !! 2) * 57 + length (corrupts !! 3) * 1197))
    print ("Part 2: " ++ show (median $ sort $ map (\x -> scoring x 0) incompletes))
