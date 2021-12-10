module Day10 (Model, pt1, pt2) where

import Data.List ( sort )

type Model = [String]

-- Part 1

pt1 :: Model -> Int
pt1 = sum . map (score . checkCorrupt [])

data Corrupt = Incomplete
             | Complete
             | Corrupt Char
             deriving stock (Show, Eq)

checkCorrupt :: String -> String -> Corrupt
checkCorrupt []     []     = Complete
checkCorrupt _      []     = Incomplete
checkCorrupt []     (x:xs) = checkCorrupt [x] xs
checkCorrupt (y:ys) (x:xs) | opener x        = checkCorrupt (x:y:ys) xs
                           | x == closeFor y = checkCorrupt ys xs
                           | otherwise       = Corrupt x

opener :: Char -> Bool
opener x = x == '(' || x == '{' || x == '[' || x == '<'

closeFor :: Char -> Char
closeFor '(' = ')'
closeFor '{' = '}'
closeFor '[' = ']'
closeFor '<' = '>'
closeFor _   = ' '

score :: Corrupt -> Int
score Incomplete = 0
score Complete   = 0
score (Corrupt c) | c == ')' = 3
                  | c == ']' = 57
                  | c == '}' = 1197
                  | c == '>' = 25137
                  | otherwise = 0

-- Part 2

pt2 :: Model -> Int
pt2 xs = head $ drop (length ts `div` 2) ts
  where
    zs = zip xs (map (checkCorrupt []) xs)
    ls = map fst (filter (\(_,y) -> not (corrupt y)) zs)
    ts = sort $ map (score2 . invert . (fix [])) ls

corrupt :: Corrupt -> Bool
corrupt (Corrupt _) = True
corrupt _ = False

fix :: String -> String -> String
fix []     []     = []
fix xs     []     = xs
fix []     (x:xs) = fix [x] xs
fix (y:ys) (x:xs) | x == closeFor y = fix ys xs
                  | otherwise       = fix (x:y:ys) xs

invert :: String -> String
invert = map f
  where
    f '(' = ')'
    f '[' = ']'
    f '{' = '}'
    f '<' = '>'
    f _   = ' '

score2 :: String -> Int
score2 = score2' 0

score2' :: Int -> String -> Int
score2' x [] = x
score2' x (')':xs) = score2' (x * 5 + 1) xs
score2' x (']':xs) = score2' (x * 5 + 2) xs
score2' x ('}':xs) = score2' (x * 5 + 3) xs
score2' x ('>':xs) = score2' (x * 5 + 4) xs
score2' x (_  :xs) = score2' x xs