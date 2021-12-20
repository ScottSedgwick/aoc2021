module Trees
  ( Tree (..)
  , Cxt (..)
  , Loc
  , left
  , right
  , top
  , up
  , upmost
  , modify
  ) where

data Tree a = Leaf a 
            | Fork (Tree a) (Tree a)
            deriving stock (Eq, Show)

data Cxt a = Top | L (Cxt a) (Tree a) | R (Tree a) (Cxt a) deriving stock (Show)

type Loc a = (Tree a, Cxt a)

left :: Loc a -> Loc a
left (Fork l r, c) = (l, L c r)
left x = x

right :: Loc a -> Loc a
right (Fork l r, c) = (r, R l c)
right x = x

top :: Tree a -> Loc a
top t = (t, Top)

up :: Loc a -> Loc a
up (t, L c r) = (Fork t r, c)
up (t, R l c) = (Fork l t, c)
up x = x

upmost :: Loc a -> Loc a
upmost l@(_, Top) = l
upmost l = upmost (up l)

modify :: Loc a -> (Tree a -> Tree a) -> Loc a
modify (t, c) f = (f t, c)
