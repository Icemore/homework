-- (2 балла)

import Control.Applicative

data Tree a = Node a [Tree a]

instance Functor Tree where
    fmap f (Node x xs) = Node (f x) (map (fmap f) xs)

instance Applicative Tree where
    pure x = Node x (repeat $ pure x)
    (Node x xs) <*> (Node y ys) = Node (x y) (zipWith (<*>) xs ys)
