data Complex = C Double Double
     deriving (Show)

instance Num (Complex) where

    (+) (C x1 x2) (C y1 y2) = C (x1 + y1) (x2 + y2)

    negate (C x1 x2) = C (-x1) (-x2)

    (*) (C x1 x2) (C y1 y2) = C (x1*y1 - x2*y2) (x1*y2 + x2*y1)

    abs (C x1 x2) = C (sqrt (x1^2 + x2^2)) 0

    signum (C x1 x2) = C (x1/(sqrt $ x1^2 + x2^2)) (x2/(sqrt $ x1^2 + x2^2))

    fromInteger a = C (fromInteger a) 0

instance Eq Complex where
    (==) (C x1 x2) (C y1 y2) = (x1 == y1) && (x2 == y2)

