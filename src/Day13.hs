module Day13 (Model, parser, pt1, pt2) where

import Control.Monad ( forM_ )
import Data.List ( nub )
import Text.Megaparsec ( optional, (<|>), many )
import Text.Megaparsec.Char ( eol )
import ParseUtils ( Parser, int, symbol )

data Fold = FoldY Int 
          | FoldX Int 
          deriving stock (Show, Eq)

data Model = Model
  { dots :: [(Int, Int)]
  , folds :: [Fold]
  } deriving stock Show

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

foldx :: Int -> [(Int, Int)] -> [(Int, Int)]
foldx l = map f
  where 
    f (x,y) = (if x > l then 2 * l - x else x, y)

foldy :: Int -> [(Int, Int)] -> [(Int, Int)]
foldy l = map f
  where 
    f (x,y) = (x, if y > l then 2 * l - y else y)

-- Part 2
-- Fold to end, figure out code

pt2 :: Model -> IO Int
pt2 m = do
  let ds = foldl (flip foldxy) (dots m) (folds m)
  let xs = [0..maximum (map fst ds)]
  let ys = [0..maximum (map snd ds)]
  forM_ ys $ \y -> do
    let l = map (\x -> if (x,y) `elem` ds then '#' else '.') xs
    print l
  pure 0

foldxy :: Fold -> [(Int, Int)] -> [(Int, Int)]
foldxy f xs = case f of
                FoldX x -> foldx x xs
                FoldY y -> foldy y xs