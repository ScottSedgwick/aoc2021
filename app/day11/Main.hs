module Main (main) where

import Day11 
import qualified Data.Map as M
import Options.Applicative ( execParser )
import Utils ( timeMe, options, Options(..) ) 

main :: IO ()
main = run =<< execParser options

run :: Options -> IO ()
run opts = timeMe $ do
  let f = if stage opts == "1" then pt1 else pt2
  xs <- readFile (infile opts)
  let ys = lines xs
  let zs = map (zip [0..]) ys
  let ws = zip [0..] zs
  let ms = foldr (\(y,ls) b -> foldr (\(x,c) b' -> M.insert (x,y) (read [c]:: Int) b') b ls) M.empty ws
  print (f ms)
