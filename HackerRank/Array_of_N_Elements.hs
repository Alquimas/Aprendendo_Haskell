fn :: Int -> [Int]
fn n = take n $ repeat 0

main = do
    n <- readLn :: IO Int
    print (fn(n))
