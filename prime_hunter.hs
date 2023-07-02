prime_hunter :: Int -> [Int]
prime_hunter n
    | n < 2 = []
    | otherwise = aux [2..n]

aux :: [Int] -> [Int]
aux [] = []
aux (x:xs) = x : (aux [a | a <- xs, mod a x /= 0])

-------------------------------------------------------------------------------
isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = not $ any (isMultiple' n) [2..sqrtX]
    where 
        sqrtX = floor (sqrt (fromIntegral n))

isMultiple' :: Int -> Int -> Bool
isMultiple' a b = mod a b == 0

firstNPrimes :: Int -> [Int]
firstNPrimes k = take k (filter isPrime [2..])
