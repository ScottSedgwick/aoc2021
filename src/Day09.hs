module Day09 (Model, parser, pt1, pt2) where

import Data.Ord ( Down(Down) )
import Data.List ( sortOn )
import qualified Data.Map as M
import ParseUtils ( Parser, digitLine )
import Text.Megaparsec ( some )

type Model = [[Int]]

parser :: Parser Model
parser = some digitLine

-- Part 1

pt1 :: Model -> Int
pt1 hs = sum $ map (\(x,y) -> 1 + getPos hs (x,y)) (getLows hs)

getLows :: Model -> [(Int, Int)]
getLows hs = filter (isLow hs) ps
  where
    xs = [0..length (head hs) - 1]
    ys = [0..length hs - 1]
    ps = [(x,y) | x <- xs, y <- ys]

isLow :: Model -> (Int, Int) -> Bool
isLow hs (x,y) = h < v
  where
    h = getPos hs (x,y)
    a = getPos hs (x - 1, y)
    b = getPos hs (x, y - 1)
    c = getPos hs (x + 1, y)
    d = getPos hs (x, y + 1)
    v = minimum [a,b,c,d]

getPos :: Model -> (Int, Int) -> Int
getPos hs (x, y) = if x < 0 || x >= length (head hs) || y < 0 || y >= length hs
                   then 1000000
                   else (hs !! y) !! x

-- Part 2

data CellState = Wall
               | Zone Int
               | Unknown
               deriving stock (Show, Eq)

data Cell = Cell
  { height :: Int
  , state :: CellState
  } deriving stock Show

type Space = M.Map (Int, Int) Cell

mkSpace :: Model -> Space
mkSpace hs = M.fromList (map (\p -> (p, mkCell (getPos hs p))) ps)
  where
    ps = [(x,y) | x <- [0..length(head hs) - 1], y <- [0..length hs - 1]]

mkCell :: Int -> Cell
mkCell 9 = Cell { height = 9, state = Wall }
mkCell n = Cell { height = n, state = Unknown }

pt2 :: Model -> Int
pt2 hs = product (map snd ss)
  where
    ls = zip [1..] (getLows hs)
    zs = map fst ls
    xs = mkSpace hs
    bs = foldr (\(z,p) b -> fillFrom z b p) xs ls
    ss = take 3 $ sortOn (Down . snd) (map (\ z -> (z, getSize z bs)) zs)

getSize :: Int -> Space -> Int
getSize z xs = M.size $ M.filter (\c -> state c == Zone z) xs

fillFrom :: Int -> Space -> (Int, Int) -> Space
fillFrom z xs p = fillFrom' z xs p (M.lookup p xs)

fillFrom' :: Int -> Space -> (Int, Int) -> Maybe Cell -> Space
fillFrom' z xs p mc =
  case mc of
    Nothing -> xs
    (Just c) -> if state c /= Unknown
                then xs
                else fill z xs p

fill :: Int -> Space -> (Int, Int) -> Space
fill z xs (x,y) = xs5
  where
    xs1 = M.adjust (\c -> c { state = Zone z }) (x,y) xs 
    xs2 = fillFrom z xs1 (x,y+1)
    xs3 = fillFrom z xs2 (x,y-1)
    xs4 = fillFrom z xs3 (x+1,y)
    xs5 = fillFrom z xs4 (x-1,y)
