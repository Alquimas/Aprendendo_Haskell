type ComplexNumber = (Double, Double) 

-- Functions than create the points in the [-2,1] X [-1.5,1.5]
--------------------------------------------------------------------------------
xAxis :: Double -> [Double]
xAxis x = [-2.0, -2.0 + 3.0 / x..1.0]

yAxis :: Double -> [Double]
yAxis y = [1.5, 1.5 - 3.0 / y..(-1.5)]

complexSection :: [Double] -> [Double] -> [[ComplexNumber]]
complexSection [] _ = []
complexSection _ [] = []
complexSection x (y:ys) = map (\a -> (a, y)) x : complexSection x ys

--------------------------------------------------------------------------------
-- Iterator
-- ActualValue -> RemainingIterations -> InitialValue -> Color
iterator :: ComplexNumber -> Double -> ComplexNumber -> String
iterator _ 0 _ = "0 0 0 "
iterator (x, y) n (z, w)
    | cardioidAndBulb (z, w) = "0 0 0 "
    | x^2 + y^2 <= 4 = iterator (x*x - y*y + z,2*x*y + w) (n-1) (z, w)
    | otherwise = convergence n

-- If the starting point is in the main cardioid or the 2-bulb, it converges
-- I tried to make this works for every iteration, but i had some problems 
-- with precision
cardioidAndBulb :: ComplexNumber -> Bool
cardioidAndBulb (x, y)
    | (x + 1)*(x + 1) + y*y <= 0.0625 = True
    | 4 * a * (a + (x - 0.25)) <= y*y = True
    | otherwise = False
    where 
        a = (x - 0.25)*(x - 0.25) + y*y

-- Functions than associate a RGB color to a specific number of iterations
-- For points than converge, they are associated with (0, 0, 0)
--------------------------------------------------------------------------------
convergence :: Double -> String
convergence n = rgb n ++ rgb (3*n) ++ rgb (5*n)

rgb :: Double -> String
rgb n = (show $ mod (floor $ ((sin n) * 255)^2) 256) ++ " "

-- The heart of the code
-- SetOfPoints -> NumberOfIterations -> SetOfColors
--------------------------------------------------------------------------------
fractal :: [Double] -> [Double] -> Double -> [[String]]
fractal x y z = (map . map) (iterator (0,0) z) $ complexSection x y

-- The idea here is to make one string and put it on the PPM archive
--------------------------------------------------------------------------------
result :: Double -> [[String]] -> String
result a x = header a ++ unlines (map oneString x)

-- [Colors] -> Colors
oneString :: [String] -> String
oneString [] = []
oneString (x:xs) = x ++ oneString xs

header :: Double -> String
header x = "P3\n" ++ (show $ floor x) ++ " " ++ (show $ floor x) ++ "\n255\n"

name :: Double -> String
name x = "mandelbrotset" ++ (show $ floor x) ++ ".ppm"
--Main
--------------------------------------------------------------------------------
main = do
    putStrLn "Enter the image resolution:"
    input1 <- getLine
    putStrLn "Enter the number of iterations:"
    input2 <- getLine
    let
        num1 = read input1 :: Double
        num2 = read input2 :: Double
    writeFile (name num2) (result (num1) (fractal (xAxis (num1 - 1)) (yAxis (num1-1)) num2))
