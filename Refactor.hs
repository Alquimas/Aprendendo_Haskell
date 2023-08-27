reader :: [String] -> String
reader [] = []
reader (x:y:z:xs) = pars y' z' (take x' xs) ++ reader (drop x' xs)
    where
        x' :: Int
        x' = read x
        y' :: Int
        y' = read y
        z' :: Int
        z' = read z
        
pars :: Int -> Int -> [String] -> String
pars x y xs = show (function x y 1 0 1 xs) ++ "\n"

function :: Int -> Int -> Int -> Int -> Int -> [String] -> Int
function _ _ a _ _ [] = a
function x y a b c (k:ks)
    | length k + b > y = 
        case (c == x) of
            True -> function x y (a + 1) (length k + 1) 1 ks
            False -> function x y a (length k + 1) (c+1) ks
    | otherwise = function x y (a) (b + (length k) + 1) c ks

main = interact
       $ reader
       . words
