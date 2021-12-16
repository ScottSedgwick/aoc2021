module Day16 (Model, parser, pt1, pt2) where

import ParseUtils ( Parser )
import Text.Megaparsec ( parse, count, some )
import Text.Megaparsec.Char ( binDigitChar )

data Model = Literal (Int, Int)
           | Operator (Int, Int, [Model])
           deriving stock (Show)

-- Parser

parser :: Parser Model
parser = do
  v <- bitsToInteger <$> count 3 binDigitChar
  t <- bitsToInteger <$> count 3 binDigitChar
  case t of
    4 -> literalP v ""
    _ -> operatorP v t

literalP :: Int -> String -> Parser Model
literalP v s = do
  cont <- count 1 binDigitChar 
  value <- count 4 binDigitChar
  if cont == "0"
    then pure $ Literal (v, bitsToInteger (s <> value))
    else literalP v (s <> value)

operatorP :: Int -> Int -> Parser Model
operatorP v t = do
  l <- bitsToInteger <$> count 1 binDigitChar 
  ps <- if l == 0
        then do
          bitcount <- bitsToInteger <$> count 15 binDigitChar 
          opdata <- count bitcount binDigitChar
          parseSome opdata
        else do
          packetcount <- bitsToInteger <$> count 11 binDigitChar
          count packetcount parser
  pure $ Operator (v, t, ps)

parseSome :: String -> Parser [Model]
parseSome xs = case parse (some parser) "" xs of
                Left _ -> pure []
                Right ps -> pure ps

bitsToInteger :: String -> Int
bitsToInteger = foldl (\b a -> b * 2 + if a == '1' then 1 else 0) 0

-- Part 1

pt1 :: Model -> Integer
pt1 = sumVersions

sumVersions :: Model -> Integer 
sumVersions (Literal (v,_)) = fromIntegral v
sumVersions (Operator (v, _, ps)) = fromIntegral v + sum (map sumVersions ps)

-- Part 2

pt2 :: Model -> Integer
pt2 = calcValue

calcValue :: Model -> Integer 
calcValue (Literal (_, v)) = fromIntegral v
calcValue (Operator (_,t,ps)) =
  case t of
    0 -> sum (map calcValue ps)
    1 -> product (map calcValue ps)
    2 -> minimum (map calcValue ps)
    3 -> maximum (map calcValue ps)
    5 -> if calcValue (head ps) > calcValue (head (tail ps)) then 1 else 0
    6 -> if calcValue (head ps) < calcValue (head (tail ps)) then 1 else 0
    7 -> if calcValue (head ps) == calcValue (head (tail ps)) then 1 else 0
    _ -> 0