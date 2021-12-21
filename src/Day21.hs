module Day21 (Model, parser, pt1, pt2) where

import qualified Data.IntMap as IM

import Control.Monad (replicateM)
import qualified Data.Array as A 
import Data.Array ((!))
import ParseUtils ( Parser, int, symbol ) 
import Text.Megaparsec.Char ( eol )

data Model = Model
  { xpos :: Int 
  , ypos :: Int 
  , xscore :: Int 
  , yscore :: Int 
  } deriving stock Show

parser :: Parser Model
parser = do
  _ <- symbol "Player 1 starting position: "
  x <- int
  _ <- eol
  _ <- symbol "Player 2 starting position: "
  y <- int
  pure Model { xpos = x, ypos = y, xscore = 0, yscore = 0 }

-- Part 1

data Roller = Roller
  { rolls :: Int 
  , nextv :: Int 
  } deriving stock Show

next :: Roller -> (Int, Roller)
next r = (nextv r, r')
  where
    x = nextv r + 1
    r' = Roller { rolls = rolls r + 1, nextv = if x > 100 then 1 else x }

next3 :: Roller -> (Int, Roller)
next3 r = (x + y + z, r''')
  where
    (x,r') = next r
    (y,r'') = next r'
    (z, r''') = next r''

pt1 :: Model -> Integer
pt1 t = fromIntegral $ playx 1000 t Roller { rolls = 0, nextv = 1 }

move :: Int -> Int -> Int 
move oldpos moves = if x > 10 then (x - 1) `mod` 10 + 1 else x
  where
    x = oldpos + moves

playx :: Int -> Model -> Roller -> Int
playx w t r 
  | yscore t >= w = xscore t * rolls r
  | otherwise     = playy w t' r'
  where
    (v,r') = next3 r
    xpos' = move (xpos t) v
    t' = t { xpos = xpos', xscore = xscore t + xpos' }

playy :: Int -> Model -> Roller -> Int
playy w t r 
  | xscore t >= w = yscore t * rolls r
  | otherwise     = playx w t' r'
  where
    (v,r') = next3 r
    ypos' = move (ypos t) v
    t' = t { ypos = ypos', yscore = yscore t + ypos' }

-- Part 2

pt2 :: Model -> Integer
pt2 t = max xs ys
  where
    (xs, ys) = scores ! (xpos t, ypos t, 0 :: Int, 0 :: Int)
    scores   = A.listArray bounds [ foldr addTpl (0, 0) [ if s1 + p1' >= 21 then (n, 0) else (y * n, x * n)
                                                        | (r, n) <- rollfreq
                                                        , let p1' = move p1 r
                                                        , let (x, y) = scores ! (p2, p1', s2, s1 + p1')
                                                        ]
                                  | (p1, p2, s1, s2) <- A.range bounds
                                  ]

bounds :: ((Int, Int, Int, Int), (Int, Int, Int, Int))
bounds   = ((1, 1, 0, 0), (10, 10, 20, 20))

rollfreq :: [(Int, Integer)]
rollfreq = IM.toAscList $ IM.fromListWith (+) [(d, 1) | d <- sum <$> replicateM 3 [1..3]]

addTpl :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addTpl (a, b) (c, d) = (a + c, b + d)