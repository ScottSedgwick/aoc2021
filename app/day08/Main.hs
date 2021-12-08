module Main (main) where

import Day08 ( Record (..), pt1, pt2 ) 
import Data.List.Split ( splitOn )
import Options.Applicative ( execParser )
import Utils ( timeMe, options, Options(..) ) 

main :: IO ()
main = run =<< execParser options

run :: Options -> IO ()
run opts = timeMe $ do
  let f = if stage opts == "1" then pt1 else pt2
  xs <- readFile (infile opts)
  let ys = lines xs
  let zs = map toRecord ys
  (print . f) zs

toRecord :: String -> Record
toRecord xs = Record { patterns = ps, output = os }
  where
    ys = splitOn " | " xs
    ps = words (head ys)
    os = words (head $ tail ys)