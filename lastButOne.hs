--Faça uma função que retorna o penultimo elemento de uma lista
lastButOne :: [a] -> a
lastButOne all
    | length all <= 1 = error "List too short: impossible to find required item"
    | length all == 2 = x
    | otherwise = lastButOne xs
    where
        x = head all
        xs = tail all
