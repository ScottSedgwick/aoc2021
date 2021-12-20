module Day19 (Model, parser, pt1, pt2) where

import Data.Either ( partitionEithers )
import Data.List ( foldl' )
import Data.Maybe ( mapMaybe )
import qualified Data.Set as S 

import ParseUtils ( Parser, int, symbol )
import Text.Megaparsec ( optional, (<?>), some )
import Text.Megaparsec.Char ( char, eol )

type Model = [Scanner]
type Point = (Int, Int, Int)
type Scanner = (Int, S.Set Point)

parser :: Parser Model
parser = some scannerP

scannerP :: Parser Scanner
scannerP = do
  _ <- symbol "--- scanner " <?> "no scanner preamble header"
  x <- int <?> "no scanner index"
  _ <- symbol " ---\n" <?> "no scanner preamble tail"
  xs <- some pointP 
  _ <- optional eol
  pure (x, S.fromList xs)

pointP :: Parser Point
pointP = do
  x <- int <?> "Point: No x value"
  _ <- char ',' <?> "Point: No xy separator"
  y <- int <?> "Point: No y value"
  _ <- char ',' <?> "Point: No yz separator"
  z <- int <?> "Point: No y value"
  _ <- optional eol
  pure (x, y, z)

-- Part 1

data Axis = X | Y | Z deriving stock (Show, Eq)

spins :: [(Int, Int, Int) -> (Int, Int, Int)]
spins = flip (foldl' f) <$> rs
  where 
    f (x, y, z) X = (x, -z, y)
    f (x, y, z) Y = (z, y, -x)
    f (x, y, z) Z = (y, -x, z)
    rs = [ [], [X], [Y], [Z], [X, X], [X, Y], [X, Z], [Y, X], [Y, Y], [Z, Y], [Z, Z], [X, X, X], [X, X, Y], [X, X, Z], [X, Y, X], [X, Y, Y], [X, Z, Z], [Y, X, X], [Y, Y, Y], [Z, Z, Z]
         , [X, X, X, Y], [X, X, Y, X], [X, Y, X, X], [X, Y, Y, Y]
         ]

matchup :: S.Set Point -> S.Set Point -> Maybe (Point, S.Set Point)
matchup adjusted points = 
  case found of
    (allAdjustedPoints, pos') : _ -> Just (pos', allAdjustedPoints)
    _                             -> Nothing
 where
  add (x, y, z) (x', y', z') = (x + x', y + y', z + z')
  sub (x, y, z) (x', y', z') = (x - x', y - y', z - z')
  found =
    [ (points', offset)
    | a <- take (S.size adjusted - 11) $ S.toList adjusted
    , spin <- spins
    , p    <- S.toList points
    , let offset  = sub a (spin p)
    , let points' = S.map (add offset . spin) points
    , S.size (S.intersection adjusted points') >= 12
    ]

matched :: [(Point, S.Set Point)] -> [(Int, S.Set Point)] -> [(Point, S.Set Point)]
matched xs [] = xs
matched xs ys = matched (xs ++ p) s
  where
    (p, s) = partitionEithers $ map f ys
    f y = case mapMaybe (\x -> matchup (snd x) (snd y)) xs of
            []    -> Right y
            (z:_) -> Left z
            
pt1 :: Model -> Int
pt1 []         = 0 
pt1 ((_,x):xs) = (S.size . S.unions . map snd) (matched [((0, 0, 0), x)] xs)

pt2 :: Model -> Int
pt2 [] = 0
pt2 ((_,x):xs) = maximum [ abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2) | (x1, y1, z1) <- ps, (x2, y2, z2) <- ps ]
  where
    ys = matched [((0, 0, 0), x)] xs
    ps = map fst ys