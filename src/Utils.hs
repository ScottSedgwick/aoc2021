module Utils ( rotate, toDec ) where

import Data.Char ( digitToInt )
import Data.List ( foldl' )

rotate :: Eq a => [[a]] -> [[a]]
rotate [] = []
rotate xs = if null ys then [] else ys : rotate zs
  where
    ys = concatMap (take 1) xs
    zs = map (drop 1) xs

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0