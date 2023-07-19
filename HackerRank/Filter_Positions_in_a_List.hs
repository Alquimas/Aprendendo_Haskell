f :: Int -> [Int] -> [Int]
f _ [] = []
f n (x:xs) 
    | n `mod` 2 == 0 = f (n+1) xs
    | otherwise = x : (f (n+1) xs)

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
    inputdata <- getContents
    mapM_ (putStrLn. show). (f 0). map read. lines $ inputdata
