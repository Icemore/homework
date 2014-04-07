import Prelude hiding(lookup)
import Test.HUnit

-- 1. fun четные числа в нечетных позициях (нумеруя с 0) умножает на 2, остальные не изменяет.
--    (0.5 балла)
fun :: [Integer] -> [Integer]
fun (x:y:t) | even y    = x : 2*y : fun t 
            | otherwise = x : y : fun t
fun x = x

-- 2. Бесконечный список Фибоначчи.
--    (0.5 балла)
fibs :: [Integer]
fibs = go 1 1
    where 
        go a b = a : go b (a+b)

-- 3a. shiftL переставляет первый элемент в конец списка. Реализуйте эту функцию так, чтобы она проходила по списку только один раз.
--     (0.5 балла)
shiftL :: [a] -> [a]
shiftL (x:xs) = xs ++ [x]
shiftL [] = []

-- 3b. shiftR переставляет последний элемент в начало. Реализуйте эту функцию так, чтобы она проходила по списку только один раз.
--     (0.5 балла)
shiftR :: [a] -> [a]
shiftR (x:xs) = ins x (shiftR xs)
    where
        ins x (y:ys) = y:x:ys
        ins x [] = [x]
shiftR [] = []

-- 4. takeLast n xs возвращает последние n элементов списка xs.
--    (0.5 балла)
takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse
-- takeLast n xs = snd $ go xs 
--     where
--         go :: [a] -> (Int, [a])
--         go (x:xs) | fst t == 0 = t
--                   | otherwise  = ((fst t) - 1, x:xs)
--             where t = go xs
--         go [] = (n, [])

-- 5. swap i j меняет местами i и j элементы.
--    (1 балл)
swap :: Int -> Int -> [a] -> [a]
swap i j xs | i > j = swap j i xs
            | j > length xs || i < 0 = xs
            | otherwise = 
                let (left, xi:tmp) = splitAt i xs
                    (mid, xj:right) = splitAt (j-i-1) tmp
                in left ++ xj:mid ++ xi:right

-- 6. Назовем элементы, которые удовлетворяют предикату p хорошими, остальные плохими.
--    Тогда mapl p f xs выбрасывает плохие элементы, а блоки подряд идущих хороших элементов,
--    которые разделяются плохими, отправляет в функцию f и возвращает список результатов.
--    Заметьте, что в функцию f никогда не передаются пустые списки.
--    (1 балл)
mapl :: (a -> Bool) -> ([a] -> b) -> [a] -> [b]
mapl p f xs =  map f (snd (go xs))
    where 
        -- go :: [a] -> (Bool, [[a]])
        go (x:xs) | p x = if wasBad then (False, [x]:ys) else (False, (x:(head ys)):(tail ys))
                  | otherwise = (True, ys)
            where (wasBad, ys) = go xs
        go [] = (True, [])

------------------------------------------------------------------------------
-- 7.

data Tree a = Node a [Tree a]

-- (a) Возвращает высоту дерева.
--     (1 балл)
height :: Tree a -> Int
height (Node _ []) = 1
height (Node _ children) = maximum (map height children) + 1

-- (b) Возвращает среднее арифметическое значений во всех узлах дерева.
--     Необходимо вычислить эту функцию, выполнив один проход по дереву.
--     (1 балл)
avg :: Tree Int -> Int
avg node = let (val, cnt) = go node in quot val cnt
    where 
        go (Node val []) = (val, 1)
        go (Node val children) = foldl add (val, 1) children
              where add (v1, cnt1) ch = let (v2, cnt2) = go ch in (v1+v2, cnt1+cnt2)
-- (c) Возвращает ширину дерева.
--     Ширина дерева определяется следующим образом:
--     Количество вершин на определенном уровне называется шириной уровня.
--     Ширина дерева - это максимальная ширина уровня по всем уровням.
--     (1 балл)
width :: Tree a -> Int
width x = maximum (go x)
    where 
        go (Node _ []) = [1]    
        go (Node _ children) = 1 : (foldl add [] (map go children))
           where 
                add xs [] = xs
                add [] ys = ys
                add (x:xs) (y:ys) = (x+y) : (add xs ys)

instance Show a => Show (Tree a) where
    show (Node val xs) = (show val) ++ ":" ++ (show xs)

instance Read a => Read (Tree a) where
    -- readsPrec :: Read a => Int -> String -> [(a, String)]
    readsPrec _ ('[':xs) = undefined 

    readsPrec _ s = concatMap (\(v, s) -> parceRest v s) (reads s)

instance Functor Tree where
    -- fmap :: (a->b) -> Tree a -> Tree b
    fmap f (Node val xs) = Node (f val) (map (fmap f) xs)

-- tests

(tree1, tree2, tree3) =
    ( b [b [l [b []],
            l [b [],
               l [b [l [],
                     l [],
                     b []],
                  l []]]],
         b [],
         b [],
         l []]
    , b [b [b [],
            b [b [],
               b []]],
         b [b [],
            l [b [],
               b []]],
         l [b []]]
    , b [tree1, tree2]
    )
  where l = Node 500; b = Node 300

(testsHeight, testsAvg, testsWidth) = (
    [ height tree1 ~?= 6
    , height tree2 ~?= 4
    , height tree3 ~?= 7
    ],
    [ avg tree1 ~?= 393
    , avg tree2 ~?= 330
    , avg tree3 ~?= 362
    ],
    [ width tree1 ~?= 4
    , width tree2 ~?= 5
    , width tree3 ~?= 7
    ])

------------------------------------------------------------------------------
-- 8. Реализовать двоичное дерево поиска без балансировки.
--    (4 балла)

data Map k v = Map k v (Maybe (Map k v)) (Maybe (Map k v))

-- Первый аргумент - функция, сравнивающая значения типа k.
-- Она вовзращает True, если первое значение меньше второго, иначе False.
lookup :: (k -> k -> Bool) -> k -> Map k v -> Maybe v
lookup p key root = go (Just root)
    where
        go (Just (Map curK curV left right))
                | p key curK = go left
                | p curK key = go right
                | otherwise = Just curV
        go Nothing = Nothing


insert :: (k -> k -> Bool) -> k -> v -> Map k v -> Map k v
insert p key val root = go (Just root)
    where
        go (Just (Map curK curV left right))
                | p key curK = Map curK curV (Just $ go left) right
                | p curK key = Map curK curV left (Just $ go right)
                | otherwise = Map curK val left right
        go Nothing = Map key val Nothing Nothing

delete :: (k -> k -> Bool) -> k -> Map k v -> Maybe (Map k v)
delete p key root = go (Just root)
    where
        go Nothing = Nothing
        go (Just (Map curK curV left right))
                | p key curK = Just $ Map curK curV (go left) right
                | p curK key = Just $ Map curK curV left (go right)
                | otherwise = merge left right
            where 
                merge Nothing y = y
                merge x Nothing = x
                merge x (Just (Map k v left right)) = Just $ Map k v (merge x left) right

fromList :: (k -> k -> Bool) -> [(k,v)] -> Map k v
fromList p ((key, val) : xs) = 
    foldl (\root (k, v) -> insert p k v root) (Map key val Nothing Nothing) xs

toList :: Map k v -> [(k,v)]
toList root = go (Just root)
    where
        go (Just (Map key val left right)) = (go left) ++ [(key, val)] ++ (go right)
        go Nothing = []

-- tests

sort :: (a -> a -> Bool) -> [a] -> [a]
sort p = map fst . toList . fromList p . map (\x -> (x, ()))

------------------------------------------------------------------------------
-- main

main = fmap (\_ -> ()) $ runTestTT $ test
    $    label "fun"
    [ fun [1,3,6,10,15,21,28,30,60] ~?= [1,3,6,20,15,21,28,60,60]
    , take 11 (fun fibs) ~?= [1,1,2,3,5,16,13,21,34,55,89]
    ] ++ label "fibs"
    [ take 10 fibs ~?= [1,1,2,3,5,8,13,21,34,55]
    , fibs !! 1000 ~?= 70330367711422815821835254877183549770181269836358732742604905087154537118196933579742249494562611733487750449241765991088186363265450223647106012053374121273867339111198139373125598767690091902245245323403501
    ] ++ label "shiftL"
    [ shiftL [1..20] ~?= [2..20] ++ [1]
    , shiftL [] ~?= ([] :: [Bool])
    ] ++ label "shiftR"
    [ shiftR [1..20] ~?= 20:[1..19]
    , shiftR [] ~?= ([] :: [Bool])
    ] ++ label "takeLast"
    [ takeLast 5 [1..20] ~?= [16,17,18,19,20]
    , takeLast 5 [1,2,3] ~?= [1,2,3]
    ] ++ label "swap"
    [ swap 1 2 [3,4,5,6] ~?= [3,5,4,6]
    , swap 2 0 "abcd" ~?= "cbad"
    , swap 100 7 [1..10] ~?= [1..10]
    ] ++ label "mapl"
    [ mapl (\x -> x `mod` 7 /= 3) id [1..20] ~?= [[1,2],[4,5,6,7,8,9],[11,12,13,14,15,16],[18,19,20]]
    , mapl (\x -> elem x [1,4,6,8,9,10,12,14,15,16,18,20]) sum [1..20] ~?= [1,4,6,27,12,45,18,20]
    ] ++ label "height" testsHeight
      ++ label "avg" testsAvg
      ++ label "width" testsWidth
      ++ label "Map" -- можете сами написать тесты на каждую функцию :)
    [ sort (<) [10,24,13,56,35,13,6,23] ~?= [6,10,13,23,24,35,56] ]
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
