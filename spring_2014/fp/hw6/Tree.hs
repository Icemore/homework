module Tree where

import Data.List
-- (2 балла)

data Tree a = Node { value :: a, children :: [Tree a] }

instance Eq a => Eq (Tree a) where
    (==) x y = if (value x == value y) then (children x == children y) else False

instance Read a => Read (Tree a) where
    readsPrec _ s = concatMap (\(v,s) -> parseRest v s) (reads s)
      where
        readList :: Read a => String -> [([Tree a], String)]
        readList ('{':'}':s) = [([], s)]
        readList ('{':s) = concatMap (\(t,s') -> insertInFront t (parseItems s')) (reads s)
            where
                insertInFront :: Tree a -> [([Tree a], String)] -> [([Tree a], String)]
                insertInFront t xs = map (\(li, s) -> (t:li, s)) xs

                parseItems :: Read a => String -> [([Tree a], String)]
                parseItems (',':s) = concatMap (\(t,s') -> insertInFront t (parseItems s')) (reads s)
                parseItems ('}':s) = [([], s)]

        parseRest :: Read a => a -> String -> [(Tree a, String)]
        parseRest v (':':s) = 
            let res = map (\(l,s') -> (Node v l,s')) (readList s)
            in case res of {[] -> [(Node v[], ':':s)]; x -> x}
        parseRest v s = [(Node v [], s)]

{-
instance Read a => Read (Tree a) where
    readsPrec _ s = concatMap (\(v,s) -> parseRest v s) (reads s)
      where
        parseRest :: Read a => a -> String -> [(Tree a, String)]
        parseRest v (':':s) = map (\(l,s') -> (Node v l,s')) (reads s)
        parseRest _ _ = []

-- read "1:[2:[4:[],5:[]],3:[]]"
-}

instance Show a => Show (Tree a) where
    show x = (show $ value x) ++ (printChildren $ children x)
        where 
            printChildren [] = ""
            printChildren xs = ":{" ++ (intercalate "," (map show xs)) ++ "}"

instance Functor Tree where
    fmap f (Node val xs) = Node (f val) (map (fmap f) xs)
