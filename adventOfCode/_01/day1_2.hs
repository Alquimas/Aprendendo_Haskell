import Control.Arrow
import Data.Char
import Data.List

main = interact
       $ words
       >>> map function
       >>> foldr (+) 0
       >>> show

function :: String -> Int
function xs = (getStart xs)*10 +
              (getEnd $ reverse xs)

getStart :: String -> Int
getStart x'@(x:xs)
    | isNumber x = (read . singleton) x
    | isPrefixOf "one" x' = 1
    | isPrefixOf "two" x' = 2
    | isPrefixOf "three" x' = 3
    | isPrefixOf "four" x' = 4
    | isPrefixOf "five" x' = 5
    | isPrefixOf "six" x' = 6
    | isPrefixOf "seven" x' = 7
    | isPrefixOf "eight" x' = 8
    | isPrefixOf "nine" x' = 9
    | otherwise = getStart xs

getEnd :: String -> Int
getEnd x'@(x:xs)
    | isNumber x = (read . singleton) x
    | isPrefixOf "eno" x' = 1
    | isPrefixOf "owt" x' = 2
    | isPrefixOf "eerht" x' = 3
    | isPrefixOf "ruof" x' = 4
    | isPrefixOf "evif" x' = 5
    | isPrefixOf "xis" x' = 6
    | isPrefixOf "neves" x' = 7
    | isPrefixOf "thgie" x' = 8
    | isPrefixOf "enin" x' = 9
    | otherwise = getEnd xs


