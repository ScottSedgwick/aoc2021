module Day01 (Model, parser, pt1, pt2) where

import ParseUtils ( Parser, intline )
import Text.Megaparsec ( many )

type Model = [Int]

parser :: Parser Model
parser = many intline

pt1 :: Model -> Int
pt1 xs = foldr (\(x,y) b -> b + if y > x then 1 else 0) 0 ys
  where
    ys = zip xs (tail xs)

pt2 :: Model -> Int
pt2 xs = foldr (\(a,b,c,d) r -> r + if ((a + b + c)::Int) < ((b + c + d)::Int) then 1 else 0) 0 ys
  where
    ys = zip4 xs (drop 1 xs) (drop 2 xs) (drop 3 xs)
    
zip4 :: [a] -> [a] -> [a] -> [a] -> [(a,a,a,a)]
zip4 [] _ _ _ = []
zip4 _ [] _ _ = []
zip4 _ _ [] _ = []
zip4 _ _ _ [] = []
zip4 (w:ws) (x:xs) (y:ys) (z:zs) = (w,x,y,z) : zip4 ws xs ys zs
