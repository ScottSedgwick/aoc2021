module Day01 (Model, parser, pt1, pt2) where

import Data.List (zip4)
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
