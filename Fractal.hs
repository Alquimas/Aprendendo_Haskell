import Data.Complex

xAxis :: Double -> [Double]
xAxis x = [-2.0, -2.0 + 4.0 / x..2.0]

yAxis :: Double -> [Double]
yAxis y = reverse [-2.0, -2.0 + 4.0 / y..2.0]

complexSection :: [Double] -> [Double] -> [[Complex Double]]
complexSection [] _ = []
complexSection _ [] = []
complexSection x (y:ys) = map (\a -> a :+ y) x : complexSection x ys

iterator :: Complex Double -> Double -> Complex Double -> Double
iterator all 0 _ = 0
iterator all@(x :+ y) n z
    | x^2 + y^2 <= 4 = iterator (all^2 + z) (n-1) z
    | otherwise = n

convergence :: Double -> String
convergence n = (show $ (floor ((sin n) * 256))^2 `mod` 256) ++ " " ++ (show $ (floor ((sin (3*n)) * 256))^2 `mod` 256) ++ " " ++ (show $ (floor ((sin (5*n))*256))^2 `mod` 256) ++ " "


convergenceMap :: [Double] -> [Double] -> [[Double]]
convergenceMap x y = (map . map) (iterator (0.0 :+ 0.0) 1000) $ complexSection x y

fractal :: [[Double]] -> [[String]]
fractal x = (map . map) convergence x

oneString :: [String] -> String
oneString [] = []
oneString (x:xs) = x ++ oneString xs

arquivo :: [[String]] -> String
arquivo x = "P3\n720 720\n255\n" ++ unlines (map oneString x)

main = writeFile "teste.ppm" (arquivo (fractal (convergenceMap (xAxis 719) (yAxis 719))))
