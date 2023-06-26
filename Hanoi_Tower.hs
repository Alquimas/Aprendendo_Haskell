type Peg = String
type Move = (Peg, Peg)

-- A B C
-- X, Y - pega a peça de X e põe em Y
--2^n - 1 moves
--1 - A, C 
--2 - A, B | A, C 
--    B, C
--3 - A, C | A, B | C, B | A, C 
--    B, A | B, C | A, C
--4 - A, B | A, C | B, C | A, B | C, A | C, B | A, B | A, C | 
--    B, C | B, A | C, A | B, C | A, B | A, C | B, C
--
--    Qual o padrão?
--    Faz o caminho anterior com B e C trocados, i.e., mover as n-1 primeiras 
--    peças para B depois A, C, i.e., mover a peça maior para C
--    Por fim o caminho anterior com A e B trocados, i.e., mover as n-1 
--    primeiras peças para C


hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n x y z
    | n < 1 = []
    | n == 1 = [(x, z)]
    | otherwise = (hanoi (n-1) x z y) ++ [(x, z)] ++ (hanoi (n-1) y x z) 

--E se tivermos A B C D? 

