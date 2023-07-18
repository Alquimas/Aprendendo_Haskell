extract :: [Int] -> Int
extract (x:[]) = x

main = interact $ unlines . (`take` (repeat "Hello World")) . extract . map read . words 
