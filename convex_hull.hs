data Direction = Left' 
    | Right'
    | Straight'
 deriving (Show)

turn :: (Num a, Ord a) => ((a, a), (a, a), (a, a)) -> Direction
turn ((x1, y1), (x2, y2), (x3, y3)) = 
    if ((x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1) == 0)
        then Straight'
        else if(((x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1) < 0))
                then Right'
                else Left'

consecutiveTurn :: (Num a, Ord a) => [(a,a)] -> [Direction]
consecutiveTurn x 
    | length x <= 2 = error "Insuffient Points"
    | otherwise = [Left']
