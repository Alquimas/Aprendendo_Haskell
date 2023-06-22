--Validating Credit Card Numbers
--
--
--Transforma um int > 0 numa lista com seus algarismos em ordem invertida
toListReverse :: Int -> [Int]
toListReverse n 
    | n <= 0 = []
    | otherwise = mod n 10 : toListReverse (div n 10)

--pega a lista da funcao anterior e inverte
toList :: Int -> [Int]
toList n = reverse (toListReverse n)

--calcula a soma dos digitios de um int > 0
digitSum :: Int -> Int
digitSum n
    | n < 10 = n
    | otherwise = k + digitSum (div n 10)
    where 
        k = mod n 10

--modifica uma lista dada a regra
modifyList :: [Int] -> [Int]
modifyList xs
    | xs == [] = []
    | mod (length xs) 2 == 1 =
        head xs : modifyList(tail xs)
    | otherwise = 
        2*(head xs) : modifyList(tail xs) 

--soma os elementos de uma lista
sumList :: [Int] -> Int
sumList xs
    | xs == [] = 0
    | otherwise = digitSum (head xs) + sumList (tail xs)

--aplica tudo acima
modifyListSum :: Int -> Int
modifyListSum n = sumList (modifyList (toList n))

--usa a anterior pra verificar o cartão
testCard :: Int -> Bool
testCard n
    | n <= 0 = False
    | otherwise = mod (modifyListSum n) 10 == 0
