module Day06 (Model, pt1, pt2) where

import qualified Data.IntMap as IM
import Data.IntMap ((!))

type Model = [Int]

pt1 :: Model -> IO Integer
pt1 = breed 80 . toIM

pt2 :: Model -> IO Integer
pt2 = breed 256 . toIM

toIM :: [Int] -> IM.IntMap Integer
toIM = foldr f empty
  where 
    f a b = IM.insertWith (+) a 1 b

empty :: IM.IntMap Integer
empty = IM.fromList $ zip [0..8] (repeat 0)

breed :: Int -> IM.IntMap Integer -> IO Integer
breed 0 xs = pure $ IM.foldr (+) 0 xs
breed n xs = breed (n - 1) (IM.insertWith (+) 6 (xs ! 0) $ IM.insert 8 (xs ! 0) ys)
  where
    ys = foldr (\a b -> IM.insert (a-1) (xs ! a) b) empty [1..8]