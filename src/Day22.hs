module Day22 (Model, parser, pt1, pt2) where

import ParseUtils ( Parser, int, symbol )
import Data.List (foldl')
import Data.Maybe ( mapMaybe )
import Control.Lens.Operators ( (^.) )
import Linear.V3 ( R1(_x), R2(_y), R3(_z), V3(..) )
import Text.Megaparsec ( (<|>), optional, some )
import Text.Megaparsec.Char ( eol )

type Model = [Command]

type Point = V3 Int
type Range3d = (Point, Point)
data Command = Command
  { on :: Bool 
  , r :: Range3d
  } deriving stock Show

parser :: Parser Model
parser = some $ onP <|> offP

onP :: Parser Command
onP = do
  _ <- symbol "on "
  r <- ptP
  pure $ Command { on = True, r = r }

offP :: Parser Command
offP = do
  _ <- symbol "off "
  r <- ptP
  pure $ Command { on = False, r = r }

ptP :: Parser Range3d
ptP = do
  _ <- symbol "x="
  x1 <- int
  _ <- symbol ".."
  x2 <- int
  _ <- symbol ",y="
  y1 <- int
  _ <- symbol ".."
  y2 <- int
  _ <- symbol ",z="
  z1 <- int
  _ <- symbol ".."
  z2 <- int
  _ <- optional eol
  pure (V3 (min x1 x2) (min y1 y2) (min z1 z2), V3 (max x1 x2) (max y1 y2) (max z1 z2))

-- Part 1

class NSquare a where
    intersect :: a -> a -> Maybe a
    area :: a -> Int

instance NSquare (Int, Int) where
    intersect (a1, a2) (b1, b2)
        | b1 > a2 || a1 > b2 = Nothing
        | otherwise          = Just (max a1 b1, min a2 b2)
    area (a1, a2) = a2 - a1 + 1

instance NSquare (Point, Point) where
    intersect (a1, a2) (b1, b2) = do
        (x1, x2) <- (a1 ^. _x, a2 ^. _x) `intersect` (b1 ^. _x, b2 ^. _x)
        (y1, y2) <- (a1 ^. _y, a2 ^. _y) `intersect` (b1 ^. _y, b2 ^. _y)
        (z1, z2) <- (a1 ^. _z, a2 ^. _z) `intersect` (b1 ^. _z, b2 ^. _z)
        return (V3 x1 y1 z1, V3 x2 y2 z2)
    area (a, b) = product (b - a + 1)

turnoff :: NSquare r => [(r, Int)] -> r -> [(r, Int)]
turnoff m r = mapMaybe isect m <> m
  where
    isect (s, m') = (, negate m') <$> (s `intersect` r)

turnon :: NSquare r => [(r, Int)] -> r -> [(r, Int)]
turnon m r = (r, 1) : turnoff m r

inBounds1 :: Point -> Bool
inBounds1 p = p ^. _x >= (-50) && p ^. _x <= 50 && p ^. _y >= (-50) && p ^. _y <= 50 && p ^. _z >= (-50) && p ^. _z <= 50

execCmd :: [(Range3d, Int)] -> Command -> [(Range3d, Int)]
execCmd m (Command True  r) = turnon m r
execCmd m (Command False r) = turnoff m r

pt1 :: Model -> IO Int
pt1 xs = do
  let ys = filter (\c -> inBounds1 (snd $ r c)) xs
  let m = foldl' execCmd [] ys
  let a = foldl' (\b (r, n) -> b + area r * n) 0 m
  pure a

pt2 :: Model -> IO Int
pt2 xs = do
  let m = foldl' execCmd [] xs
  let a = foldl' (\b (r, n) -> b + area r * n) 0 m
  pure a