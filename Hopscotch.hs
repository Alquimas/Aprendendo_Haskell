skips :: [a] -> [[a]]
skips [] = []
skips all@(x:xs) = all : skips xs
