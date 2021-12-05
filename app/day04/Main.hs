module Main (main) where

import Day04 
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let inputfile = head args
  let stage = head $ tail args
  xs <- readFile inputfile
  let f = if stage == "1"
          then pt1 
          else pt2 
  x <- f xs
  print x