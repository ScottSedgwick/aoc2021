module Day13 (Model, parser, pt1, pt2) where

import Data.List ( nub )
import Text.Megaparsec ( optional, (<|>), many )
import Text.Megaparsec.Char ( eol )
import ParseUtils ( Parser, int, symbol )
import Utils ( printPlot )

data Fold = FoldY Int 
          | FoldX Int 
          deriving stock (Show, Eq)

data Model = Model
  { dots :: [(Int, Int)]
  , folds :: [Fold]
  } deriving stock Show

-- Parser 

parser :: Parser Model
parser = do
  dots <- many dotParser
  _ <- eol
  folds <- many foldParser
  pure $ Model { dots = dots, folds = folds }

dotParser :: Parser (Int, Int)
dotParser = do
  x <- int
  _ <- symbol ","
  y <- int
  _ <- eol
  pure (x, y)

foldParser :: Parser Fold
foldParser = do
  _ <- symbol "fold along "
  foldXParser <|> foldYParser

foldXParser :: Parser Fold
foldXParser = do
  _ <- symbol "x="
  x <- int
  _ <- optional eol
  pure $ FoldX x

foldYParser :: Parser Fold
foldYParser = do
  _ <- symbol "y="
  y <- int
  _ <- optional eol
  pure $ FoldY y

-- Part one
-- Fold once, count dots (unfilled spaces)

pt1 :: Model -> IO Int
pt1 xs = do
  let ys = foldxy (head (folds xs)) (dots xs)
  pure $ length $ nub ys

foldxy :: Fold -> [(Int, Int)] -> [(Int, Int)]
foldxy f xs = 
  case f of
    FoldX x -> map (fx x) xs
    FoldY y -> map (fy y) xs
  where
    fx l (x,y) = (if x > l then 2 * l - x else x, y)
    fy l (x,y) = (x, if y > l then 2 * l - y else y)

-- Part 2
-- Fold to end, figure out code

pt2 :: Model -> IO Int
pt2 m = do
  printPlot (foldl (flip foldxy) (dots m) (folds m))
  pure 0