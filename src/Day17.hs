module Day17 (Model, parser, pt1, pt2) where

import ParseUtils ( int, symbol, Parser )
import Data.Maybe ( mapMaybe )

data Model = Model
  { xmin :: Int 
  , xmax :: Int
  , ymin :: Int
  , ymax :: Int
  } deriving stock (Show)

parser :: Parser Model
parser = do
  _ <- symbol "target area: x="
  x1 <- int
  _ <- symbol ".."
  x2 <- int
  _ <- symbol ", y="
  y1 <- int
  _ <- symbol ".."
  y2 <- int
  pure $ Model { xmin = min x1 x2, xmax = max x1 x2, ymin = min y1 y2, ymax = max y1 y2 }

-- Part 1

data Probe = Probe
  { x :: Int
  , y :: Int
  , dx :: Int
  , dy :: Int
  } deriving stock (Show)

pt1 :: Model -> Int
pt1 xs = do
  let spds = [(sx, sy) | sx <- [1..100], sy <- [1..100]]
  let zs = map highest $ mapMaybe (\(sx, sy) -> fire xs (Probe 0 0 sx sy) []) spds
  maximum zs

fire :: Model -> Probe -> [Probe] -> Maybe [Probe]
fire (Model xmin xmax ymin ymax) (Probe x y dx dy) ps
  | x >= xmin && x <= xmax && y >= ymin && y <= ymax = Just (Probe x y dx dy : ps)
  | y < ymin = Nothing
  | otherwise = fire (Model xmin xmax ymin ymax) p2 (p2:ps)
    where
       p2 = Probe { x = x + dx, y = y + dy, dx = max (dx - 1) 0, dy = dy - 1 }

highest :: [Probe] -> Int
highest = maximum . map y

-- Part 2

pt2 :: Model -> Int
pt2 xs = do
  let spds = [(sx, sy) | sx <- [-500..500], sy <- [-500..500]]
  let zs = mapMaybe (\(sx, sy) -> fire xs (Probe 0 0 sx sy) []) spds
  length zs