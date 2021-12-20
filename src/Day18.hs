module Day18 (Model, parser, pt1, pt2) where

import ParseUtils 
import Text.Megaparsec
import Text.Megaparsec.Char
import Trees

type Model = [Number]

type Number = Tree Int

--Parser 

parser :: Parser Model
parser = many modelP
  
modelP :: Parser Number
modelP = do
  n <- pairP 0
  _ <- optional eol
  pure n

pairP :: Int -> Parser Number
pairP n = do
  _ <- symbol "["
  l <- litP <|> pairP (n+1)
  _ <- symbol ","
  r <- litP <|> pairP (n+1)
  _ <- symbol "]"
  pure (Fork l r)

litP :: Parser Number
litP = Leaf <$> int

-- Part 1

pt1 :: Model -> Int
pt1 = magnitude . reduce . foldl1 addsn 

addsn :: Number -> Number -> Number
addsn x y = reduce $ Fork x y

reduce :: Number -> Number
reduce xs 
  | hasEx  = reduce ys
  | hasSp  = reduce zs
  | otherwise = xs
  where
    (hasEx, ys) = explode xs
    (hasSp, zs) = split xs
               
explode :: Number -> (Bool, Number)
explode s = (b,t)
  where
    (b, t, _, _) = explode' 0 s

explode' :: Int -> Number -> (Bool, Number, Int, Int)
explode' _ s@(Leaf _) = (False, s, 0, 0)
explode' d s@(Fork (Leaf a) (Leaf b)) 
  | d >= 4    = (True, Leaf 0, a, b)
  | otherwise = (False, s, 0, 0)
explode' d s@(Fork a b) 
  | lhsEx     = (True, Fork lhs (addL b lhsRight), lhsLeft, 0)
  | rhsEx     = (True, Fork (addR a rhsLeft) rhs, 0, rhsRight)
  | otherwise = (False, s, 0, 0)
  where
    (lhsEx, lhs, lhsLeft, lhsRight) = explode' (d+1) a
    (rhsEx, rhs, rhsLeft, rhsRight) = explode' (d+1) b 

addL :: Number -> Int -> Number
addL (Leaf m)   n = Leaf (n+m)
addL (Fork a b) n = Fork (addL a n) b

addR :: Number -> Int -> Number
addR (Leaf m)   n = Leaf (n+m)
addR (Fork a b) n = Fork a (addR b n)

split :: Number -> (Bool, Number)
split (Leaf n) 
  | n < 10    = (False, Leaf n)
  | otherwise = (True,  Fork (Leaf (n `div` 2)) (Leaf ((n+1) `div` 2)))
split (Fork a b) 
  | lhsSp     = (True,  Fork lhs b)
  | rhsSp     = (True,  Fork a rhs)
  | otherwise = (False, Fork a b)
  where 
    (lhsSp, lhs) = split a
    (rhsSp, rhs) = split b
                        
magnitude :: Number -> Int 
magnitude (Leaf x)   = x
magnitude (Fork x y) = magnitude x * 3 + magnitude y * 2

-- Part 2

pt2 :: Model -> Int
pt2 xs = maximum [(magnitude . uncurry addsn) (x,y) | x <- xs, y <- xs, x /= y]