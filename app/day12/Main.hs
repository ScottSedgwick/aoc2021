module Main (main) where

import Day12 ( pt1, pt2, load )
import Options.Applicative ( execParser )

import Utils ( timeMe, options, Options(..) ) 

main :: IO ()
main = run =<< execParser options

run :: Options -> IO ()
run opts = timeMe $ do
  let f = if stage opts == "1" then pt1 else pt2
  xs <- lines <$> readFile (infile opts)
  let m = load xs
  print $ f m