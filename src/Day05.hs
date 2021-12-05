module Day05 (Model, parser, pt1, pt2) where

import qualified Data.IntMap as IM
import ParseUtils ( Parser, eolv, int )
import Text.Megaparsec ( many, (<|>), MonadParsec(eof) )
import Text.Megaparsec.Char ( string )

type Point = (Int, Int)
type Line  = (Point, Point)
type Model = [Line]

parser :: Parser Model
parser = many lineParser

lineParser :: Parser Line
lineParser = do
  p1 <- pointParser
  _ <- string " -> "
  p2 <- pointParser
  _ <- eof <|> eolv 
  pure (p1, p2)

pointParser :: Parser Point
pointParser = do
  x <- int
  _ <- string ","
  y <- int
  pure (x,y)

pt1 :: Model -> IO Int
pt1 xs = do
  let ys = filter (\l -> horiz l || verti l) xs
  let zs = foldr fillLine IM.empty ys
  pure $ count2 zs

count2 :: IM.IntMap Int -> Int
count2 = IM.foldr (\a b -> if a > 1 then b + 1 else b) 0

fillLine :: Line -> IM.IntMap Int -> IM.IntMap Int 
fillLine l m = if horiz l then fillHLine l m else fillVLine l m

fillHLine :: Line -> IM.IntMap Int -> IM.IntMap Int 
fillHLine ((x,y1), (_,y2)) m = foldr (\y m' -> fillPoint (x,y) m') m [min y1 y2..max y1 y2]

fillVLine :: Line -> IM.IntMap Int -> IM.IntMap Int 
fillVLine ((x1,y), (x2,_)) m = foldr (\x m' -> fillPoint (x,y) m') m [min x1 x2..max x1 x2]

fillPoint :: Point -> IM.IntMap Int -> IM.IntMap Int
fillPoint p = IM.insertWith (+) (ptToPos p) 1

ptToPos :: Point -> Int 
ptToPos (x, y) = x * 1000 + y

horiz :: Line -> Bool 
horiz ((x1,_),(x2,_)) = x1 == x2

verti :: Line -> Bool 
verti ((_,y1),(_,y2)) = y1 == y2

pt2 :: Model -> IO Int
pt2 xs = do
  let ys = foldr fillLine2 IM.empty xs
  pure $ count2 ys

fillLine2 :: Line -> IM.IntMap Int -> IM.IntMap Int 
fillLine2 l m 
  | horiz l   = fillHLine l m 
  | verti l   = fillVLine l m
  | otherwise = fillDLine l m


fillDLine :: Line -> IM.IntMap Int -> IM.IntMap Int 
fillDLine ((x1,y1), (x2,y2)) m = foldr (\(x,y) m' -> fillPoint (x,y) m') m ps
  where
    ps = zip xs ys
    xs = if x1 < x2 then [x1..x2] else reverse [x2..x1]
    ys = if y1 < y2 then [y1..y2] else reverse [y2..y1]