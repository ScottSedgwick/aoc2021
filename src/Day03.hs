module Day03 (Model, pt1, pt2) where

import Data.List ( group, sort )
import Utils ( rotate, toDec )

type Model = String

pt1 :: [Model] -> IO Int
pt1 xs = do
  let ys = rotate xs
  let gamma = toDec $ map most ys
  let epsilon = toDec $ map least ys
  pure $ gamma * epsilon

most :: String -> Char
most xs = if length ones >= length zeros then '1' else '0'
  where
    ys = group $ sort xs 
    zeros = head ys
    ones = head $ tail ys

least :: String -> Char
least xs = if most xs == '1' then '0' else '1' 

pt2 :: [Model] -> IO Int
pt2 xs = do
  let o2 = filt 0 most xs
  let co2 = filt 0 least xs
  pure $ o2 * co2
    
filt :: Int -> (String -> Char) -> [Model] -> Int
filt _ _ []  = -1
filt _ _ [x] = toDec x
filt n f xs  = filt (n + 1) f ys
  where
    ys = filter (nthCharMatch n fs) xs
    fs = map f (rotate xs)

nthCharMatch :: Int -> String -> String -> Bool 
nthCharMatch n xs ys = take 1 (drop n xs) == take 1 (drop n ys)
