-- Declaracao de variaveis 
x :: Int
x = 3

-- x = 4
-- retirar o comentário da linha acima vai causar um erro
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
