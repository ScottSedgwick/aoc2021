module Main (main) where

import Day05 
import ParseUtils (parseFromFile)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let inputfile = head args
  let stage = head $ tail args
  result <- parseFromFile parser inputfile
  case result of
    Left e -> do
      putStrLn "Error: " 
      print e
    Right xs -> do
      let f = if stage == "1"
              then pt1 
              else pt2
      x <- f xs 
      print x