module Day15 (Model, parser, pt1, pt2) where

import ParseUtils ( Parser, parseGrid )
import Utils ( dijkstra, printGrid )

type Model = [[Int]]
type Cost = Int
type Node = (Int, Int)

-- Parser

parser :: Parser Model
parser = parseGrid

-- Part 1

pt1 :: Model -> IO Int
pt1 xs = do
  printGrid xs
  (pure . runDijkstra) xs

runDijkstra :: Model -> Int
runDijkstra xs = 
  case dijkstra (moveFunc dest xs) dest (0, (0,0)) of
    Nothing    -> -10
    Just (x,_) -> x 
  where
    dest = (length (head xs) - 1, length xs - 1)

moveFunc :: Node -> Model -> (Cost, Node) -> [(Cost, Node)]
moveFunc ms xs (c, n) = map (getNodeCost c xs) $ filter (isLegal ms) (moves n)

moves :: Node -> [Node]
moves (x,y) = [(x-1,y),(x,y-1),(x+1,y),(x,y+1)]

isLegal :: Node -> Node -> Bool
isLegal (mx, my) (x,y) = x >= 0 && x <= mx && y >= 0 && y <= my

getNodeCost :: Cost -> Model -> Node -> (Cost, Node)
getNodeCost c xs (x,y) = (c + (xs !! y) !! x, (x, y))

-- Part 2

pt2 :: Model -> IO Int
pt2 = pure . runDijkstra . multiplyMap 

multiplyMap :: Model -> Model
multiplyMap xs = concatMap (addAll (map multiplyLine xs)) [0..4]

multiplyLine :: [Int] -> [Int]
multiplyLine xs = concatMap (add xs) [0..4]

add :: [Int] -> Int -> [Int]
add xs x = map (inc x) xs

inc :: Int -> Int -> Int
inc x y = if z > 9 then z - 9 else z
  where
    z = x + y

addAll :: Model -> Int -> Model
addAll xs n = map (`add` n) xs