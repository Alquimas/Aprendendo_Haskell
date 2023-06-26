myDrop :: Int -> [a] -> [a]
myDrop n xs 
    | n <= 0 || null xs = xs 
    | otherwise = myDrop (n - 1) (tail xs)

firstN :: Int -> [a] -> [a]
firstN n xs 
    | n <= 0 || null xs = []
    | otherwise = (head xs) : (firstN (n - 1) (tail xs))

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

toPalindrome :: [a] -> [a]
toPalindrome [] = []
toPalindrome x = x ++ myReverse x

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x
    | myLength x <= 1 = True
    | otherwise = myDrop (size' + mod') x == halfOfList2
        where mod' = mod (myLength x) 2
              size' = (div (myLength x) 2)
              halfOfList2 = myReverse (firstN size' x)

-------------------------------------------------------------------------------

listSum :: (Num p) => [p] -> p
listSum [] = 0
listSum (x:xs) = x + listSum xs

meanList :: [Double] -> Double
meanList [] = error "empty list"
meanList x = (listSum x) / (fromIntegral (myLength x))
