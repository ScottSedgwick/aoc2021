module Day25 (Model, parser, pt1, pt2) where

import Control.Monad
import qualified Data.IntSet as IS
import Data.List
import ParseUtils ( Parser )
import Text.Megaparsec
import Text.Megaparsec.Char

data Model = Model
  { easts :: IS.IntSet
  , souths :: IS.IntSet
  , maxX :: Int
  , maxY :: Int
  } deriving stock (Show, Eq)

parser :: Parser Model
parser = do
  xs <- some lineP
  let my = length xs - 1
  let mx = length (head xs) - 1
  let easts = foldl' (flddirn '>') IS.empty (zip [0..] xs)
  let souths = foldl' (flddirn 'v') IS.empty (zip [0..] xs)
  pure $ Model easts souths mx my

lineP :: Parser String
lineP = do
  xs <- some (char '.' <|> char 'v' <|> char '>')
  _ <- optional eol
  pure xs

flddirn :: Char -> IS.IntSet -> (Int, String) -> IS.IntSet
flddirn c s (y,xs) = foldl' (\b (x,c') -> (if c == c' then addPosn x y else id) b) s (zip [0..] xs)

addPosn :: Int -> Int -> IS.IntSet -> IS.IntSet 
addPosn x y = IS.insert $ fromPt (x, y)

fromPt :: (Int, Int) -> Int
fromPt (x,y) = y * 1000 + x

toPt :: Int -> (Int, Int)
toPt p = (p `mod` 1000, p `div` 1000)

-- Part 1

pt1 :: Model -> Int
pt1 = checkMove 1

checkMove :: Int -> Model -> Int
checkMove x m 
  | m == m'   = x
  | otherwise = checkMove (x + 1) m'
  where
    m' = move m

move :: Model -> Model
move m = Model e s (maxX m) (maxY m)
  where
    e = IS.foldr (fE (maxX m, maxY m) (easts m) (souths m)) IS.empty (easts m)
    s = IS.foldr (fS (maxX m, maxY m) e (souths m)) IS.empty (souths m)

fE :: (Int, Int) -> IS.IntSet -> IS.IntSet -> Int -> IS.IntSet -> IS.IntSet 
fE (mx, _) es ss k s = 
  if k' `IS.notMember` es && k' `IS.notMember` ss
    then IS.insert k' s
    else IS.insert k s
  where
    (x,y) = toPt k
    x' = if (x + 1) > mx then 0 else x + 1
    k' = fromPt (x', y)

fS :: (Int, Int) -> IS.IntSet -> IS.IntSet -> Int -> IS.IntSet -> IS.IntSet 
fS (_, my) es ss k s = 
  if k' `IS.notMember` es && k' `IS.notMember` ss
    then IS.insert k' s
    else IS.insert k s
  where
    (x,y) = toPt k
    y' = if (y + 1) > my then 0 else y + 1
    k' = fromPt (x, y')

prtModel :: Model -> IO()
prtModel m = do
  forM_ [0..maxY m] $ \y -> do
    putStrLn $ map (\x -> ptToChr x y m) [0.. maxX m]
  putStrLn ""

ptToChr :: Int -> Int -> Model -> Char
ptToChr x y m 
  | k `IS.member` easts  m = '>'
  | k `IS.member` souths m = 'v'
  | otherwise = '.'
  where 
    k = fromPt (x, y)

-- Part 2

pt2 :: Model -> Int
pt2 = undefined