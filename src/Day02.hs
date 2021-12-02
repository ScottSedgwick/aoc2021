module Day02 (Model, parser, pt1, pt2) where

import Text.Megaparsec ( (<|>), many )
import Text.Megaparsec.Char ( string )
import ParseUtils ( Parser, intline ) 

data Model = Forward Int
           | Down Int
           | Up Int 
           | WTF
           deriving stock Show

parser :: Parser [Model]
parser = many modelParser

modelParser :: Parser Model
modelParser = do
  dn <- pDirn
  ds <- intline 
  let mdl | dn == "forward " = Forward ds
          | dn == "down "    = Down ds
          | dn == "up "      = Up ds
          | otherwise        = WTF
  pure mdl

pDirn :: Parser String
pDirn = string "forward "
    <|> string "up "
    <|> string "down "

pt1 :: [Model] -> Int
pt1 xs = forwards xs * (downs xs - ups xs)

forwards :: [Model] -> Int
forwards []             = 0
forwards (Forward x:xs) = x + forwards xs
forwards (_:xs)         = forwards xs

downs :: [Model] -> Int
downs []          = 0
downs (Down x:xs) = x + downs xs
downs (_:xs)      = downs xs

ups :: [Model] -> Int
ups []        = 0
ups (Up x:xs) = x + ups xs
ups (_:xs)    = ups xs

data Submarine = Submarine
  { aim :: Int
  , fwd :: Int
  , depth :: Int
  } deriving stock Show

pt2 :: [Model] -> Int
pt2 = pt2' (Submarine { aim = 0, fwd = 0, depth = 0})

pt2' :: Submarine -> [Model] -> Int
pt2' s [] = fwd s * depth s
pt2' s (x:xs) = pt2' (f x) xs
  where
    f (Forward y) = s { fwd = fwd s + y, depth = depth s + aim s * y }
    f (Down y)    = s { aim = aim s + y }
    f (Up y)      = s { aim = aim s - y }
    f _           = s