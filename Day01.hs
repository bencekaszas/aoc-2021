module Day01 where

first :: [Int] -> Int
first xs = length [ a | (a,b) <- zip xs (tail xs), b > a]

second :: [Int] -> Int
second = first . splitThree

splitThree :: [Int] -> [Int]
splitThree (x:y:z:[]) = [(x + y + z)]
splitThree (x:y:z:xs) = (x + y + z) : splitThree (y:z:xs)

main = do
    contents <- readFile "inputs/01.txt"
    let ss = lines contents
        xs = map read ss :: [Int]
    print ("#1: " ++ show (first xs))
    print ("#2: " ++ show (second xs))
