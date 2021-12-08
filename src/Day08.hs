module Day08 (Record(..), Model, pt1, pt2) where

import Data.List ( sort )
import qualified Data.Map as M
import Data.Map ((!))

data Record = Record 
  { patterns :: [String]
  , output :: [String]
  } deriving stock Show

type Model = [Record]

pt1 :: Model -> Int
pt1 = sum . map countUnique 

countUnique :: Record -> Int
countUnique = length . filter uniqueLength . output 

uniqueLength :: String -> Bool
uniqueLength x = l == 2 || l == 4 || l == 3 || l == 7
  where
    l = length x

pt2 :: Model -> Int
pt2 = sum . map solve

solve :: Record -> Int
solve (Record ps os) = listToDec $ map (value . sort . applyMap m) os
  where
    m = buildMap (map sort ps)

applyMap :: M.Map Char Char -> String -> String
applyMap m = map (m !)

value :: String -> Int
value "ABCEFG" = 0
value "CF" = 1
value "ACDEG" = 2
value "ADEFG" = 2
value "ACDFG" = 3
value "BCDF" = 4
value "ABDFG" = 5
value "ABCDG" = 5
value "ABDEFG" = 6
value "ABCDEG" = 6
value "ACF" = 7
value "ABCDEFG" = 8
value "ABCDFG" = 9
value _ = -1000

buildMap :: [String] -> M.Map Char Char
buildMap xs = M.fromList 
    [ (a, 'A')
    , (b, 'B')
    , (c, 'C')
    , (d, 'D')
    , (e, 'E')
    , (f, 'F')
    , (g, 'G')
    ]
  where
    one   = head $ filter (\x -> length x == 2) xs
    seven = head $ filter (\x -> length x == 3) xs
    four  = head $ filter (\x -> length x == 4) xs
    eight = head $ filter (\x -> length x == 7) xs

    zerosixnine  = filter (\x -> length x == 6) xs

    nine  = head $ filter (contains four) zerosixnine
    zero  = head $ filter (\x -> contains one x && not (contains four x)) zerosixnine

    a = head (seven `minus` one)
    b = head ((four `minus` one) `minus` [d])
    c = head one
    d = head (four `minus` zero)
    e = head (eight `minus` nine)
    f = head (tail one)
    g = head (eight `minus` [a,b,c,d,e,f])

minus :: String -> String -> String
minus xs ys = filter (`notElem` ys) xs

contains :: String -> String -> Bool 
contains xs ys = all (`elem` ys) xs

listToDec :: [Int] -> Int
listToDec = foldl (\b a -> b * 10 + a) 0
