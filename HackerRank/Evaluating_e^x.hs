exp' :: Double -> Double
exp' x = aux x 0

aux :: Double -> Int -> Double
aux _ 10 = 0
aux x n = (x^n)/(fromIntegral (fat n)) + aux x (n+1)

fat :: Int -> Int
fat 0 = 1
fat n = n * fat (n-1)

unwords' :: [String] -> String
unwords' [] =  ""
unwords' ws =  foldr1 (\w s -> w ++ '\n':s) ws


main = interact $ unwords' . map (show . exp' . read) . tail . words
