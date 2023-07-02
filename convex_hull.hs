data Direction = Left' | Right' | Straight'
 deriving (Show, Eq)

type Coordinate = (Double, Double)

direction :: (Coordinate, Coordinate, Coordinate) -> Direction
direction ((x1, y1), (x2, y2), (x3, y3)) 
    | crossProduct == 0 = Straight'
    | crossProduct < 0 = Right'
    | otherwise = Left'
    where 
        crossProduct = (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1)

consecutiveDirection :: [Coordinate] -> [Direction]
consecutiveDirection (x:y:z:ls) = direction (x, y, z) : consecutiveDirection ls
consecutiveDirection _ = []

greaterCoord :: Coordinate -> Coordinate -> Bool
greaterCoord (x1, y1) (x2, y2)
    | (y1 > y2) || (y1 == y2) && (x1 > x2) = True
    | (y1 < y2) || (y1 == y2) && (x1 < x2) = False
    | otherwise = True

sortCoord :: [Coordinate] -> [Coordinate]
sortCoord [] = []
sortCoord (x:xs) = smaller ++ [x] ++ greater
    where
        smaller = sortCoord [a | a <- xs, greaterCoord x a]
        greater = sortCoord [a | a <- xs, greaterCoord a x]

angle :: Coordinate -> Coordinate -> Double
angle (x1, y1) (x2, y2)
    | x1 == x2 && y1 == y2 = error "Equal points"
    | otherwise = dx / len
    where 
        dx = x1 - x2
        dy = y1 - y2
        len = sqrt (dx^2 + dy^2)

sortByAngle :: Coordinate -> [Coordinate] -> [Coordinate]
sortByAngle _ [] = []
sortByAngle a (x:xs) = smaller ++ [x] ++ greater
    where 
        smaller = sortByAngle a [b | b <- xs, (angle a b) <= (angle a x)]
        greater = sortByAngle a [b | b <- xs, (angle a b) > (angle a x)]


grahamScan :: [Coordinate] -> [Coordinate] -> [Coordinate]
grahamScan x y
    | null y = grahamScan xs (cx:bx:ax:[])
    | direction (cy, by, ay) == Right' = grahamScan x (ay:cy:ys)
    | null x = y 
    | otherwise = grahamScan (tail x) (ax:y)
    where
        (ax : bx : cx : xs) = x
        (ay : by : cy : ys) = y

allAtOnce :: [Coordinate] -> [Coordinate]
allAtOnce x
    | length x <= 3 = x
    | otherwise = grahamScan (y:points) []
    where 
        points = sortByAngle y ys
        y = head (sortCoord x)
        ys = tail (sortCoord x)

