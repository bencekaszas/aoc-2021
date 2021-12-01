module Day01 where


-- Part 1: find all incremental steps
first :: [Int] -> Int
first xs = length [ a | (a,b) <- zip xs (tail xs), b > a]


-- Part 2: split the list of data into 3-number windows, and then perform the
-- same process as in Part 1.
second :: [Int] -> Int
second = first . splitThree

splitThree :: [Int] -> [Int]
splitThree (x:y:z:[]) = [(x + y + z)]
splitThree (x:y:z:xs) = (x + y + z) : splitThree (y:z:xs)


-- Reading in  the input
main = do
    inps <- readFile "inputs/01.txt"
    let strs = lines inps
        xs = map read strs :: [Int]
    print ("#1: " ++ show (first xs))
    print ("#2: " ++ show (second xs))
