show' :: [Int] -> String
show' (x:y:[]) = (show x) ++ " " ++ (show y)

result :: ([Int], [Int]) -> [Int] -> [Int]
result ([],[]) a = a
result ((x:xs), (y:ys)) (a:b:[])
    | x > y = result (xs, ys) $ (a+1):b:[]
    | x < y = result (xs, ys) $ a:(b+1):[]
    | otherwise = result (xs, ys) $ a:b:[]

main = interact $ show' . (`result` [0, 0]) . (splitAt 3) . map read . words

