import Control.Arrow
import Data.Char
import Data.List

main = interact
       $ words
       >>> map (filter isNumber)
       >>> map function
       >>> foldr (+) 0
       >>> show

function :: String -> Int
function xs = (read $ (singleton . head) xs)*10 +
              (read $ (singleton. last) xs)
