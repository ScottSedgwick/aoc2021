module Main (main) where

import Day02 
import ParseUtils (parseFromFile)

main :: IO ()
main = do
  let inputfile = "data/day02/pt1.txt"
  result <- parseFromFile parser inputfile
  case result of
    Left e -> do
      putStrLn "Error: " 
      print e
    Right xs -> do
      let x = pt1 xs
      print x