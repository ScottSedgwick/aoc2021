module Day02 (Model, parser, pt1, pt2) where

import Data.List (foldl')
import Text.Megaparsec ( (<|>), many )
import Text.Megaparsec.Char ( string )
import ParseUtils ( Parser, intline ) 

data Model = Forward Int
           | Down Int
           | Up Int
           deriving stock Show

parser :: Parser [Model]
parser = many modelParser

modelParser :: Parser Model
modelParser = pModel "forward " Forward
          <|> pModel "down " Down
          <|> pModel "up " Up

pModel :: String -> (Int -> Model) -> Parser Model
pModel s m = string s >> m <$> intline

data Sub1 = Sub1
  { sub1Forward :: Int
  , sub1Depth :: Int
  } deriving stock Show

pt1 :: [Model] -> Int
pt1 = g . foldl' f (Sub1 { sub1Forward = 0, sub1Depth = 0 })
  where
    f s (Forward x) = s { sub1Forward = sub1Forward s + x }
    f s (Down x)    = s { sub1Depth   = sub1Depth   s + x }
    f s (Up x)      = s { sub1Depth   = sub1Depth   s - x }
    g s = sub1Forward s * sub1Depth s

data Sub2 = Sub2
  { sub2Aim :: Int
  , sub2Forward :: Int
  , sub2Depth :: Int
  } deriving stock Show

pt2 :: [Model] -> Int
pt2 = g . foldl' f (Sub2 { sub2Aim = 0, sub2Forward = 0, sub2Depth = 0})
  where
    f s (Forward y) = s { sub2Forward = sub2Forward s + y, sub2Depth = sub2Depth s + sub2Aim s * y }
    f s (Down y)    = s { sub2Aim = sub2Aim s + y }
    f s (Up y)      = s { sub2Aim = sub2Aim s - y }
    g s             = sub2Forward s * sub2Depth s