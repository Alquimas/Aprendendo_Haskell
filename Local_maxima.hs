localMaxima :: [Int] -> [Int]
localMaxima l 
    | length l < 3 = []
    | otherwise = 
    if y > z && y > x
    then y : localMaxima (y:z:xs) 
    else localMaxima (y:z:xs)
        where (x:y:z:xs) = l 
