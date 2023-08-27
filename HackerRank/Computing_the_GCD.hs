function :: [Int] -> Int
function (x:y:_)
    | mod x y == 0 = y
    | mod y x == 0 = x
    | (x > y) = function ((mod x y):y:[])
    | otherwise = function ((mod y x):x:[]) 

main = interact
       $ (++"\n")
       . show
       . function
       . (\(x:y:_) -> (x:y:[]))
       . map read
       . words
