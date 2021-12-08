module Day07 (Model, parser, pt1, pt2) where

import ParseUtils ( Parser, ints )

type Model = [Int]

parser :: Parser Model
parser = ints ","

pt1 :: Model -> Int
pt1 xs = minimum (map (`cost` xs) [minimum xs .. maximum xs])

cost :: Int -> Model -> Int
cost p = sum . map (\x -> abs (x - p))

pt2 :: Model -> Int
pt2 xs = minimum (map (`cost2` xs) [minimum xs .. maximum xs])

cost2 :: Int -> Model -> Int
cost2 p = sum2 . map (\x -> abs (x - p))

sum2 :: [Int] -> Int
sum2 = foldr (\a b -> sum [1..a] + b) 0