main = do
    contents <- readFile "inputs/DAYOF.txt"
    let ss = lines contents
    print ("#1: " ++ show (ss))
    print ("#2: " ++ show (f xs))
