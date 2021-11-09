module Day01 (Model, parser, pt1, pt2) where

import ParseUtils ( Parser, intline )
import Text.Megaparsec ( many )

type Model = [Int]

parser :: Parser Model
parser = do
  p <- many intline
  pure p

pt1 :: Model -> Int
pt1 xs = head [a * b | a <- xs, b <- xs, a + b == 2020] 

pt2 :: Model -> Int
pt2 xs = head [a * b * c | a <- xs, b <- xs, c <- xs, a + b + c == 2020] 
