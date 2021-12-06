module Main (main) where

import Day06 ( pt1, pt2 ) 
import Control.Monad ( (>=>) )
import Data.List.Split
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let inputfile = head args
  let stage = head $ tail args
  let f = if stage == "1" then pt1 else pt2
  xs <- readFile inputfile
  let ys = map (\x -> read x :: Int) $ splitOn "," xs
  (f >=> print) ys