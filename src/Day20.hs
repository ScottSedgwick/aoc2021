module Day20 (Model, parser, pt1, pt2) where

import ParseUtils ( Parser )
import Data.Array
import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char

type Image = M.Map (Int, Int) Bool 

data Model = Model
  { iea :: Array Int Bool
  , image :: Image
  , minx :: Int
  , maxx :: Int 
  , miny :: Int 
  , maxy :: Int
  } deriving stock (Show)

buf :: Int
buf = 110

increase :: Int
increase = 50

parser :: Parser Model
parser = do
  xs <- some spotP
  _ <- eol
  ys <- some spotLineP
  pure $ Model 
    { iea = listArray (0,511) xs
    , image = fillFalse (length (head ys)) (length ys) (imgToMap 1 ys)
    , minx = 0
    , maxx = length (head ys)
    , miny = 0
    , maxy = length ys
    }

fillFalse :: Int -> Int -> Image -> Image
fillFalse mx my img = foldr (`M.insert` False) img [(x,y) | x <- [-buf..(-1)]<>[mx+1..mx+buf], y <- [-buf..(-1)]<>[my+1..my+buf]]

spotP :: Parser Bool 
spotP = do
  x <- char '.' <|> char '#'
  pure (x == '#')

spotLineP :: Parser [Bool]
spotLineP = do
  _ <- eol
  some spotP

imgToMap :: Int -> [[Bool]] -> Image
imgToMap _ [] = M.empty
imgToMap y (xs:xxs) = foldr f (imgToMap (y+1) xxs) (zip [1..] xs) 
  where
    f (x,v) = M.insert (x,y) v 

-- Part 1

pt1 :: Model -> IO Int
pt1 xs = do
  let zs = foldr (\_ b -> enhance b) xs [1..2]
  pure $ lit zs

lit :: Model -> Int
lit m = foldr f 0 [(x,y) | x <- [minx m - increase..maxx m + increase], y <- [miny m - increase..maxy m + increase]]
  where 
    f (x,y) b = b + (case M.lookup (x, y) (image m) of
                      Just True -> 1
                      _ -> 0)
      

enhance ::  Model -> Model
enhance m = do
  let cs = [(x,y) | x <- [minx m - buf..maxx m + buf], y <- [miny m - buf..maxy m + buf]]
  m { image = foldr (\k b -> M.insert k (calcVal k m) b) M.empty cs }

calcVal :: (Int, Int) -> Model -> Bool 
calcVal (x,y) (Model iea img _ _ _ _) = iea ! i
  where
    cs = [(x-1, y-1), (x, y-1), (x+1,y-1), (x-1, y), (x,y), (x+1,y), (x-1, y+1), (x, y+1), (x+1, y+1) ]
    bs = map (`M.lookup` img) cs
    i = bitsToInt $ reverse bs

bitsToInt :: [Maybe Bool] -> Int
bitsToInt [] = 0
bitsToInt (Nothing:xs) = 2 * bitsToInt xs
bitsToInt ((Just False):xs) = 2 * bitsToInt xs
bitsToInt ((Just True):xs) = 1 + 2 * bitsToInt xs

prtImage :: Model -> IO()
prtImage m = mapM_ (prtImgLine m) [-buf..maxy m + buf] 

prtImgLine :: Model -> Int -> IO()
prtImgLine m y = do
  let bs = map (\x -> image m M.! (x,y)) [-buf..maxx m + buf]
  let s = map (\b -> if b then '#' else '.') bs
  putStrLn s

-- Part 2

pt2 :: Model -> IO Int
pt2 xs = do
  let zs = foldr (\_ b -> enhance b) xs [1..50]
  pure $ lit zs