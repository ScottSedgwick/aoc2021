module Main (main) where

import Day07 ( parser, pt1, pt2 )

import Options.Applicative ( execParser )
import ParseUtils (parseFromFile)
import Utils ( timeMe, options, Options(..) ) 

main :: IO ()
main = run =<< execParser options

run :: Options -> IO ()
run opts = timeMe $ do
  let f = if stage opts == "1" then pt1 else pt2
  xs <- parseFromFile parser (infile opts)
  either print (print . f) xs