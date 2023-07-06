intersperse' :: a -> [[a]] -> [a]
intersperse' _ [] = []
intersperse' _ (x:[]) = x
intersperse' sep (x:y:[]) = x ++ [sep] ++ y

