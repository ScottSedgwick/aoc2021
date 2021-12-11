module Day11 (Model, pt1, pt2) where

import qualified Data.Map as M

type Model = M.Map (Int, Int) Int

-- Part 1

pt1 :: Model -> Int
pt1 = powerup 100 0

powerup :: Int -> Int -> Model -> Int
powerup 0 m _  = m
powerup n m xs = do
  let ys = M.map (+1) xs
  let zs = flash ys
  let m' = m + M.size (M.filter (<1) zs)
  let ws = M.map (\x -> if x < 0 then 0 else x) zs
  powerup (n - 1) m' ws

flash :: Model -> Model
flash xs | M.size fs == 0 = xs
         | otherwise      = do
           let ys = M.foldrWithKey (\p _ b -> flashcell p b) xs fs
           flash ys
  where
    fs = M.filter (>9) xs

flashcell :: (Int, Int) -> Model -> Model
flashcell (x,y) xs = M.insert (x,y) (-100) xs8
  where
    xs1 = M.adjust (+1) (x-1,y) xs
    xs2 = M.adjust (+1) (x+1,y) xs1
    xs3 = M.adjust (+1) (x,y-1) xs2
    xs4 = M.adjust (+1) (x,y+1) xs3
    xs5 = M.adjust (+1) (x-1,y-1) xs4
    xs6 = M.adjust (+1) (x+1,y-1) xs5
    xs7 = M.adjust (+1) (x-1,y+1) xs6
    xs8 = M.adjust (+1) (x+1,y+1) xs7

-- Part 2

pt2 :: Model -> Int
pt2 = findsync 1

findsync :: Int -> Model -> Int
findsync n xs = do
  let ys = M.map (+1) xs
  let zs = flash ys
  if M.size (M.filter (<1) zs) == M.size zs
    then n
    else findsync (n + 1) (M.map (\x -> if x < 0 then 0 else x) zs)
