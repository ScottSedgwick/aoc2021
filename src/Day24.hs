module Day24 (Model, Step, parser, pt1, pt2) where

import Control.Monad.State ( MonadState(get), State, evalState, modify )
import Data.List.Split ( chunksOf )
import qualified Data.Set as S

type Step = (Int, Int, Int)
type Model = [Step]

parser :: String -> IO Model
parser filename = map parseStep . chunksOf 18 . map words . lines <$> readFile filename

parseStep :: [[String]] -> Step
parseStep input = (read x, read y, read z)
  where
    x = input !! 5 !! 2
    y = input !! 4 !! 2
    z = input !! 15 !! 2

-- Part 1

solve :: [Int] -> Int -> Model -> State (S.Set (Int, Int)) [[Int]]
solve _ _ [] = pure []
solve ws z (s : steps) = do
  dp <- get
  if S.member (z, length steps) dp
    then pure []
    else do
      result <- concat <$> traverse (\w -> f w (step z s w)) ws
      modify (S.insert (z, length steps))
      pure result
  where
    f w z' = if null steps
             then (if z' == 0 then pure [[w]] else pure [])
             else map (w :) <$> solve ws (step z s w) steps

step :: Integral p => p -> (p, p, p) -> p -> p
step z (addX, divZ, addY) w = if x /= w then (z' * 26) + (w + addY) else z'
  where
    x  = (z `rem` 26) + addX
    z' = z `quot` divZ 

pt1 :: Model -> [Int]
pt1 steps = head $ evalState (solve [9, 8 .. 1] 0 steps) S.empty

-- Part 2

pt2 :: Model -> [Int]
pt2 steps = head $ evalState (solve [1 .. 9] 0 steps) S.empty