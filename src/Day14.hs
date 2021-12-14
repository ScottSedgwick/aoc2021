module Day14 (Model, parser, pt1, pt2) where

import Data.Char ( ord )
import Data.List ( foldl', sort )
import qualified Data.IntMap as M
import ParseUtils ( eolv, strline, symbol, Parser )
import Text.Megaparsec ( optional, many, some )
import Text.Megaparsec.Char ( eol, letterChar )

type Tuples = M.IntMap Integer
type Rules = M.IntMap (Int, Int)
data Model = Model
  { tuples :: Tuples
  , rules :: Rules
  , lastc :: Int
  } deriving stock Show

-- Parser

parser :: Parser Model
parser = do
  t <- strline
  _ <- eolv
  rs <- many ruleParser
  pure $ Model { tuples = getTplFreq t, rules = M.fromList rs, lastc = ord $ last t}

ruleParser :: Parser (Int, (Int, Int))
ruleParser = do
  s <- some letterChar
  _ <- symbol " -> "
  t <- ord <$> letterChar
  _ <- optional eol
  let x = ord $ head s
  let y = ord $ head $ tail s
  pure (x * 100 + y, (x * 100 + t, t * 100 + y))

strToInt :: String -> Int
strToInt [] = 0
strToInt [_] = 0
strToInt (x:y:_) = ord x * 100 + ord y

getTplFreq :: String -> Tuples
getTplFreq xs = foldr (\(x,y) b -> M.alter f (strToInt [x,y]) b) M.empty ys
  where
    ys = zip xs (tail xs)
    f Nothing = Just 1
    f (Just n) = Just (n + 1)

-- Part 1

pt1 :: Model -> Integer
pt1 xs = score (lastc xs) ys
  where
    ys = polymeriseN 10 (rules xs) (tuples xs)

polymeriseN :: Int -> Rules -> Tuples -> Tuples
polymeriseN n rs xs = foldl' (\b _ -> polymerise rs b) xs [1..n]

polymerise :: Rules -> Tuples -> Tuples
polymerise rs ts = ts2
  where
    ts1 = M.foldlWithKey' (addRules rs) ts ts
    ts2 = M.foldlWithKey' (subRules rs) ts1 ts

addRules :: Rules -> Tuples -> Int -> Integer -> Tuples
addRules rs ts k v = 
  case M.lookup k rs of
    Nothing    -> ts
    Just (x,y) -> M.alter f x $ M.alter f y ts
  where
    f Nothing = Just v
    f (Just x) = Just (x + v)

subRules :: Rules -> Tuples -> Int -> Integer -> Tuples
subRules rs ts k v = 
  case M.lookup k rs of
    Nothing -> ts
    Just _  -> M.alter f k ts
  where
    f Nothing  = Nothing
    f (Just x) = Just (x - v)

score :: Int -> Tuples -> Integer
score c xs = last zs - head zs
  where
    zs = sort $ map snd $ M.toList $ scores c xs

scores :: Int -> Tuples -> M.IntMap Integer
scores c xs = zs
  where 
    ys = M.foldrWithKey f M.empty xs
    zs = M.adjust (+1) c ys 
    f k a b = M.alter (g a) (k `div` 100) b
    g a Nothing  = Just a
    g a (Just b) = Just (a + b)

-- Part 2

pt2 :: Model -> Integer
pt2 xs = score (lastc xs) ys
  where
    ys = polymeriseN 40 (rules xs) (tuples xs)
