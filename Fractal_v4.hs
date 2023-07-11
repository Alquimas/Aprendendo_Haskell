import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import System.IO

type ComplexNumber = (Double, Double)
type Color = (Int, Int, Int)

-- Functions than create the points in the [-2,1] X [-1.5,1.5]
--------------------------------------------------------------------------------
xAxis :: Double -> [Double]
xAxis x = [-2.0, -2.0 + 3.0 / x..1.0]

yAxis :: Double -> [Double]
yAxis y = [1.5, 1.5 - 3.0 / y..(-1.5)]

complexSection :: Double -> Double -> [ComplexNumber]
complexSection a b = [(x, y) | y <- yAxis b, x <- xAxis a]

--------------------------------------------------------------------------------
-- Iterator
-- ActualValue -> RemainingIterations -> InitialValue -> Color
iterator :: ComplexNumber -> Double -> ComplexNumber -> Color
iterator _ 0 _ = (0, 0, 0)
iterator (x, y) n (z, w)
    | cardioidAndBulb (z, w) = (0,0,0)
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
convergence :: Double -> Color
convergence n = (rgb n, rgb (3*n), rgb (5*n))

rgb :: Double -> Int
rgb n = mod (floor $ ((sin n) * 255)^2) 256

-- The heart of the code
-- ListOfPoints -> NumberOfIterations -> SetOfColors
--------------------------------------------------------------------------------
fractal :: [ComplexNumber] -> Double -> [Int]
fractal [] _ = []
fractal (l:ls) a = x : y : z : (fractal ls a)
    where
        (x, y, z) = iterator (0,0) a l

intListToByteString :: [Int] -> L.ByteString
intListToByteString = L8.pack . map (toEnum . fromIntegral)

name :: Double -> Double -> String
name x y = (show $ floor x) ++ "_mandelbrotset" ++ (show $ floor y) ++ ".ppm"
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
    outh <- openFile (name num1 num2) WriteMode
    hPutStrLn outh ("P6\n" ++ (show $ floor num1) ++ " " ++ (show $ floor num1) ++ "\n255")
    L.hPut outh (intListToByteString (fractal (complexSection (num1-1) (num1-1)) num2)) 
    hClose outh 
