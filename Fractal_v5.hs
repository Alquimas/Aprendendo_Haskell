import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import System.IO

type ComplexNumber = (Double, Double)
type Color = (Int, Int, Int)

-- Functions than create the points
--------------------------------------------------------------------------------
xAxis :: Double -> Double -> Double -> [Double]
xAxis res x rad = [(x - rad), (x - rad) + (2 * rad) / res..(x + rad)]

yAxis :: Double -> Double -> Double -> [Double]
yAxis res y rad = [(y + rad), (y + rad) - (2 * rad) / res..(y - rad)]

complexSection :: Double -> Double -> Double -> Double -> [ComplexNumber]
complexSection res rad xC yC = [(x, y) | y <- yAxis res yC rad, x <- xAxis res xC rad]

--------------------------------------------------------------------------------
-- Iterator
-- ActualValue -> RemainingIterations -> InitialValue -> Color
iterator :: ComplexNumber -> Double -> ComplexNumber -> Color
iterator _ 0 _ = (0, 0, 0)
iterator (x, y) n (z, w)
    | cardioidAndBulb (z, w) = (0,0,0)
    | x^2 + y^2 <= 4 = iterator (x*x - y*y + z,2*x*y + w) (n - 1) (z, w)
    | otherwise = convergence n

-- If the starting point is in the main cardioid or the 2-bulb, it converges
-- Only works for the mandelbrot set
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
rgb n = mod (floor $ n^2) 256

-- The heart of the code
-- ListOfPoints -> NumberOfIterations -> ListOfColors
--------------------------------------------------------------------------------
fractal :: [ComplexNumber] -> Double -> [Int]
fractal [] _ = []
fractal (l:ls) ite = x : y : z : (fractal ls ite)
    where
        (x, y, z) = iterator (0,0) ite l

-- Convert a list of ints into a ByteString
--------------------------------------------------------------------------------
intListToByteString :: [Int] -> L.ByteString
intListToByteString = L8.pack . map (toEnum . fromIntegral)

-- Create the name of the file
--------------------------------------------------------------------------------
name :: Double -> Double -> Double -> Double -> String
name x y xC yC = (show $ floor x) ++ "_" ++ (show xC) ++ "_" ++ (show yC) ++ "_" ++ (show $ floor y) ++ ".ppm"

--Main
--------------------------------------------------------------------------------
main = do
    putStrLn "Enter the image resolution:"
    input1 <- getLine
    putStrLn "Enter the number of iterations:"
    input2 <- getLine
    putStrLn "Enter the x coordinate of the center:"
    input3 <- getLine
    putStrLn "Enter the y coordinate of the center:"
    input4 <- getLine
    putStrLn "Enter the radius of the image:"
    input5 <- getLine
    let
        res = read input1 :: Double
        ite = read input2 :: Double
        xC = read input3 :: Double
        yC = read input4 :: Double
        rad = read input5 :: Double
    outh <- openFile (name res ite xC yC) WriteMode
    hPutStrLn outh ("P6\n" ++ (show $ floor res) ++ " " ++ (show $ floor res) ++ "\n255")
    L.hPut outh (intListToByteString (fractal (complexSection (res-1) (rad) (xC) (yC)) ite)) 
    hClose outh 
