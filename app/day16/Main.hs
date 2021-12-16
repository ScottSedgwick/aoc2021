module Main (main) where

import Day16 ( parser, pt1, pt2 )
import Options.Applicative ( execParser )
import Utils ( timeMe, options, Options(..) ) 
import Text.Megaparsec ( runParser, Parsec, ParseErrorBundle )

main :: IO ()
main = run =<< execParser options

run :: Options -> IO ()
run opts = timeMe $ do
  let f = if stage opts == "1" then pt1 else pt2
  xs <- parseFromFile parser (infile opts)
  either print (print . f) xs

parseFromFile :: Parsec e String a -> String -> IO (Either (ParseErrorBundle String e) a)
parseFromFile p file = do
  xs <- readFile file
  let ys = foldr (\a b -> toBin a <> b) [] xs
  pure $ runParser p file ys 

toBin :: Char -> String 
toBin '0' = "0000"
toBin '1' = "0001"
toBin '2' = "0010"
toBin '3' = "0011"
toBin '4' = "0100"
toBin '5' = "0101"
toBin '6' = "0110"
toBin '7' = "0111"
toBin '8' = "1000"
toBin '9' = "1001"
toBin 'A' = "1010"
toBin 'B' = "1011"
toBin 'C' = "1100"
toBin 'D' = "1101"
toBin 'E' = "1110"
toBin 'F' = "1111"
toBin _   = "XXXX"