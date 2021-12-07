module Day07 (Model, parser, pt1, pt2) where

import ParseUtils ( Parser, ints )

type Model = [Int]

parser :: Parser Model
parser = ints ","

pt1 :: Model -> IO Int
pt1 xs = pure $ minimum (map (cost xs) [minimum xs .. maximum xs])

cost :: Model -> Int -> Int
cost xs p = sum $ map (\x -> abs (x - p)) xs

pt2 :: Model -> IO Int
pt2 xs = pure $ minimum (map (cost2 xs) [minimum xs .. maximum xs])

cost2 :: Model -> Int -> Int
cost2 xs p = sum2 $ map (\x -> abs (x - p)) xs

sum2 :: [Int] -> Int
sum2 = foldr (\a b -> sum [1..a] + b) 0