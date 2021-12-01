module Main (main) where

import Day21 
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
      let x = if stage == "1"
              then pt1 xs
              else pt2 xs
      print x