module Day04 (Model, pt1, pt2) where

import qualified Data.IntMap as IM
import Data.Maybe ( isNothing )
import Data.List.Split ( splitOn )

data Model = Model
  { nums :: [Int]
  , boards :: IM.IntMap Board
  } deriving stock Show

type Board = IM.IntMap (Maybe Int)

type Posn = (Int, Int)

posnToInt :: Posn -> Int
posnToInt (r,c) = r * 10 + c

parseBoard :: String -> Board
parseBoard xs = buildBoard 0 cs
  where
    rs = splitOn "\n" xs
    cs = map words rs

buildBoard :: Int -> [[String]] -> Board
buildBoard _ [] = IM.empty
buildBoard r (x:xs) = buildRow r 0 x (buildBoard (r + 1) xs)

buildRow :: Int -> Int -> [String] -> Board -> Board
buildRow _ _ []     b = b
buildRow r c (x:xs) b = IM.insert (posnToInt (r,c)) (Just (read x :: Int)) (buildRow r (c + 1) xs b)

pt1 :: String -> IO Int
pt1 xs = do
  let ys = splitOn "\n\n" xs
  let ns = map (\x -> read x :: Int) $ splitOn "," (head ys)
  let bs = map parseBoard (tail ys)
  findWinner 0 ns bs 

findWinner :: Int -> [Int] -> [Board] -> IO Int 
findWinner _ [] _ = pure 0
findWinner y (x:xs) bs = do
  let ws = filter winner bs
  if not (null  ws)
  then processWinner y (head ws)
  else findWinner x xs (map (processNumber x) bs)

processWinner :: Int -> Board -> IO Int
processWinner x b = do
    let s = IM.foldr f 0 b 
    pure (x * s)
  where  
    f Nothing  n = n
    f (Just m) n = m + n

processNumber :: Int -> Board -> Board
processNumber n = IM.map (\x -> if x == Just n then Nothing else x)

winner :: Board -> Bool 
winner b = rw || cw
  where
    rw = any (rowWinner b) [0..4]
    cw = any (colWinner b) [0..4]

rowWinner :: Board -> Int -> Bool
rowWinner b r = all (\c -> isNothing ((IM.!) b (posnToInt (r,c)))) [0..4]

colWinner :: Board -> Int -> Bool
colWinner b c = all (\r -> isNothing ((IM.!) b (posnToInt (r,c)))) [0..4]

pt2 :: String -> IO Int
pt2 xs = do
  let ys = splitOn "\n\n" xs
  let ns = map (\x -> read x :: Int) $ splitOn "," (head ys)
  let bs = map parseBoard (tail ys)
  findLoser 0 ns bs 

findLoser :: Int -> [Int] -> [Board] -> IO Int 
findLoser _ [] _      = pure 0
findLoser _ (x:_) [b]     = processWinner x (processNumber x b)
findLoser _ (x:xs) bs = do
  let cs = map (processNumber x) bs
  findLoser x xs (filter (not . winner) cs)