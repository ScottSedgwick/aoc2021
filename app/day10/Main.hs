module Main (main) where

import Day10 ( pt1, pt2 ) 
import Options.Applicative ( execParser )
import Utils ( timeMe, options, Options(..), getLines ) 

main :: IO ()
main = run =<< execParser options

run :: Options -> IO ()
run opts = timeMe $ do
  let f = if stage opts == "1" then pt1 else pt2
  xs <- getLines opts
  print $ f xs