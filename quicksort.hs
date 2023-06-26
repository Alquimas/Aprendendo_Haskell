quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort menorEq ++ [x] ++ quicksort maior
    where menorEq = [a | a <- xs, a <= x]
          maior = [a | a <- xs, a > x]

quicksortList :: [[a]] -> [[a]]
quicksortList [] = []
quicksortList (x:xs) = quicksortList menorEq ++ [x] ++ quicksortList maior
    where menorEq = [a | a <- xs, length a <= length x]
          maior = [a | a <- xs, length a > length x]


