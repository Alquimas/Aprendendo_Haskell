map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) 
    | p x = x : filter' p xs
    | otherwise = filter' p xs

foldl' :: (a  -> b -> a) -> a -> [b] -> a
foldl' step acc (x:xs) = foldl' step (step acc x) xs
foldl' _ acc [] = acc 

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' step acc (x:xs) = step x (foldr' step acc xs)
foldr' _ acc [] = acc

-------------------------------------------------------------------------------
-- definição da função ++
myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x:xs) ys = x : myAppend xs ys

-- definição da função head
myHead :: [a] -> a
myHead [] = error "Lista Vazia"
myHead (x:_) = x

-- definição da função last 
myLast :: [a] -> a
myLast [] = error "Lista Vazia"
myLast [x] = x
myLast (_:xs) = myLast xs

-- definição da função tail 
myTail :: [a] -> [a]
myTail [] = error "Lista Vazia"
myTail (_:xs) = xs

-- definição da função init 
myInit :: [a] -> [a]
myInit [] = error "Lista Vazia"
myInit [x] = []
myInit (x:xs) = x : myInit xs

-- definição da função length
myLength :: [a] -> Int 
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- definição da função null 
myNull :: [a] -> Bool
myNull [] = True 
myNull _ = False

-- definição da função !!
myIndex :: [a] -> Int -> a
myIndex xr n 
    | myNull xr = error "Lista Vazia" 
    | n < 0 = error "Indice negativo"
    | n + 1 > myLength xr = error "Indice muito grande"
    | n == 0 = x
    | otherwise = myIndex xs (n - 1)
    where
        (x:xs) = xr

-- definição da função reverse
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- definição da função and
myAnd :: [Bool] -> Bool
myAnd = foldr' (&&) True 

-- definição da função or 
myOr :: [Bool] -> Bool
myOr = foldr' (||) False

-- definição da função any 
myAny :: (a -> Bool) -> [a] -> Bool
myAny f x = myOr $ map' f x 

-- definição da função all 
myAll :: (a -> Bool) -> [a] -> Bool
myAll f x = myAnd $ map' f x 

-- definição da função concat
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

-- definição da função scanl 
myScanl :: (b -> a -> b) -> b -> [a] -> [b]
myScanl _ acc [] = [acc]
myScanl step acc (x:xs) = acc : myScanl step (step acc x) xs

-- definição da função scanr
myScanr :: (a -> b -> b) -> b -> [a] -> [b]
myScanr _ acc [] = [acc]
myScanr step acc (x:xs) = step x q : qs
    where 
        qs@(q:_) = myScanr step acc xs

-- definição da função iterate
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

-- definição da função repeat 
myRepeat :: a -> [a]
myRepeat x = x : myRepeat x

-- definição da função take 
myTake :: Int -> [a] -> [a]
myTake n l
    | myNull l = []
    | n < 1 = []
    | otherwise = x : myTake (n - 1) xs
    where 
        (x:xs) = l

-- definição da função replicate 
myReplicate :: Int -> a -> [a]
myReplicate n x 
    | n < 1 = []
    | otherwise = myTake n $ myRepeat x

-- definição da função cycle 
myCycle :: [a] -> [a]
myCycle [] = error "Lista Vazia"
myCycle x = xs
    where
        xs = x ++ xs 



