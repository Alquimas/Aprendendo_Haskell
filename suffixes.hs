import Data.List

parsing :: [String] -> String
parsing ("0":"0":_) = []
parsing (x:y:xs) = solve a b ++ "\n"
                   ++ parsing c
    where
        a = take (read x) xs
        b = take (read y) (drop (read x) xs)
        c = drop (read y) (drop (read x) xs)

solve :: [String] -> [String] -> String
solve a b = show (length $ nub' y)
    where
        y = [ z ++ z' | z <- a', z' <- b']
        a' = concat $ map inits a
        b' = concat $ map tails b

nub' :: [String] -> [String]
nub' xs = map head $ (group . sort) xs

main = interact
       $ parsing
       . words
