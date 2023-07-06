-- Declaracao de variaveis 
x :: Int
x = 3

-- x = 4
-- retirar o comentário da linha acima vai causar um erro
--
--
--Tipos
--
-- Int : Inteiros limitados
--
-- Integer : Inteiros ilimitados
--
-- Float: Numero de ponto flutuante de precisao simples
--
-- Double: Numero de ponto flutuante de precisao dupla
--
-- Bool: Booleanos
--
-- Char: Caracters Unicode
--
-- String: Listas de caracters
--
-- Conversao
--
-- fromIntegral : Int, Integer → Num
-- round, floor, ceiling → ponto flutuante pra Int, Integer
--
-- Operadores 
--
-- + só opera mesmo tipo, logo Int + Integer dá erro
-- / é apenas para ponto flutuante
-- para inteiros se usa div a b, ou a `div` b
--
--
-- Definindo funcoes 
--
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial(n-1)

--retorna o resultado da operacao hailstone em n
hailstone :: Integer -> Integer
hailstone n
    | mod n 2 == 0 = div n 2
    | otherwise    = 3*n + 1

--retorna a sequencia de hailstone comecando em n
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

--retorna o tamanho de uma lista
intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs

--retorna o tamanho da sequencia de hailstone comecando em n
hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1

--retorna o tamanho de todas as sequencias de hailstone de n ate 0
hailstoneSeqLen :: Integer -> [Integer]
hailstoneSeqLen 0 = []
hailstoneSeqLen n = hailstoneLen(n) : hailstoneSeqLen(n - 1)


-- tipo de dados Algébrico 
-- Basicamente um tipo de dado personalizado 
--
-- data AlgDataType = Const1 Type11 Type12
--                  | Const2 Type21 
--                  | Const3 Type31 Type32 Type 33
--
--Nota: TIPOS e CONSTRUTORES precisam começar com letra MAIUSCULA 
--Enquanto variaveis E nome de funções com letra MINUSCULA
--]
-- árvore 
--data Tree = Leaf Char
--   | Node Tree Int Tree
--deriving Show
--
--
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) 
    | p x = x : filter' p xs
    | otherwise = filter' p xs

--lambdas 
main = print $ filter' (\xs -> length xs > 15) (map hailstoneSeqLen [1..100])

foldl' :: (a  -> b -> a) -> a -> [b] -> a
foldl' step acc (x:xs) = foldl' step (step acc x) xs
foldl' _ acc [] = acc 

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' step acc (x:xs) = step x (foldr' step acc xs)
foldr' _ acc [] = acc

--existem também foldl1 e foldr1, que tratam o inicio/fim da lista como 
--acumuladores. Devido a isso, elas quebram numa lista vazia

myFoldl' :: (a -> b -> a) -> a -> [b] -> a
myFoldl' f z xs = (foldr' step id xs) z
    where 
        step x g a = g (f a x)


--type variables podem ser também funções, e é ai que vem a sacada
--foldr' step id xs recebe uma lista de tipo [b] e uma função do tipo
--a -> a e portanto deve retornar uma função do tipo a -> a 
--em cada passo do folding, só damos x e g a step, de modo que ele continua
--necessitando do termo a para ser executado  

--podemos declarar uma classe do seguinte modo 
--class NOME VARIAVEIS where 
--  <funcoes que toda instancia de NOME precisa implementar>

-- para que um tipo criado seja derivado de uma classe, i.e., sendo uma
-- instancia dessa classe 
-- podemos usar <INDENT>deriving (Class1,Class2,...), desde que a classe
-- seja escrita em termos de outras que já pertençam as classes
-- ou podemos 
--instance Class type where 
--    <defina aqui todas as funcoes basicas que a classe precisa>
--    <para todos os seus construtores>
