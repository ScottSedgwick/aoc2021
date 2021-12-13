module Day12 (Model, load, pt1, pt2) where

import Data.Char ( isLower, isUpper )
import Data.List ( group, sort )
import Data.List.Split ( splitOn )
import qualified Data.Map as M

type Model = M.Map String [String]

-- Parser 

load :: [String] -> Model
load = foldr f M.empty
  where 
    f x m = insert y z (insert z y m)
      where
        xs = splitOn "-" x
        y = head xs
        z = head $ tail xs

insert :: String -> String -> Model -> Model
insert a b m 
  | a == "end" || b == "start" = m
  | otherwise =
    case M.lookup a m of
      Nothing -> M.insert a [b] m
      Just xs -> M.insert a (b:xs) m

-- Part 1

pt1 :: Model -> Int
pt1 = length . allPaths visitedCheck1 [] "start"

visitedCheck1 :: String -> [String] -> Bool 
visitedCheck1 x xs = isLower (head x) && x `elem` xs

allPaths :: (String -> [String] -> Bool) -> [String] -> String -> Model -> [[String]]
allPaths f vs node graph 
  | node == "end"  = [["end"]]
  | f node vs      = []
  | otherwise      =
  case M.lookup node graph of
    Nothing -> []
    Just [] -> [[node]]
    Just xs -> map (node:) (concatMap (\x -> allPaths f (node:vs) x graph) xs)

-- Part 2

pt2 :: Model -> Int
pt2 = length . allPaths visitedCheck2 [] "start"

visitedCheck2 :: String -> [String] -> Bool 
visitedCheck2 x xs 
  | isUpper (head x) = False 
  | x `notElem` xs   = False
  | otherwise = maximum (map length zs) > 1
  where
    ys = filter (isLower . head) xs
    zs = group (sort ys)