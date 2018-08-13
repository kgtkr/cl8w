module Wasm.IndexedList where

type IndexedList a=[(Int,a)]

length :: IndexedList a -> Int
length []           = 0
length ((i, _) : _) = i

cons :: a -> IndexedList a -> IndexedList a
cons x []                = [(1, x)]
cons x list@((i, _) : _) = (i + 1, x) : list

toList :: IndexedList a -> [a]
toList = map snd

toIndexed :: [a] -> IndexedList a
toIndexed = foldr f []
  where
    f x []                = [(1, x)]
    f x list@((i, _) : _) = (i + 1, x) : list
