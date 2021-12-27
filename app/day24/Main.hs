module Main (main) where

import Day24 ( parser, pt1, pt2 )
import Data.Char ( intToDigit )

main :: IO ()
main = do
  steps <- parser "data/day24/data.txt"
  putStrLn $ map intToDigit $ pt1 steps
  putStrLn $ map intToDigit $ pt2 steps