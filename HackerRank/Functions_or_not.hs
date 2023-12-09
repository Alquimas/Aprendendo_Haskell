import Data.List

parsing :: [Int] -> String
parsing (x:xs) = function x xs

function :: Int -> [Int] -> String
function 0 _ = []
function x (y:ys) = solve a ++ function (x-1) b
    where
        a = makePairs $ take (2*y) ys
        b = drop (2*y) ys
        
makePairs :: [Int] -> [(Int, Int)]
makePairs [] = []
makePairs (x:y:xs) = (x,y) : makePairs xs
        
solve :: [(Int, Int)] -> String
solve xs
    | length (nubBy (\(a, _) (b, _) -> a == b) ys) == 
    length ys = "YES\n"
    | otherwise = "NO\n"
    where
        ys = nub xs

main = interact
       $ parsing
       . map read
       . words
