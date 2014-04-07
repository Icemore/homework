import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Data.List as L

-- (2 балла)

-- 1. Определить класс типов MapLike.
--    В нем должны быть функции empty, lookup, insert, delete, fromList с типами как в Data.Map.
--    Напишите реализацию по умолчанию для fromList.

-- 2. Определить instance MapLike для (a) Data.Map, (b) ListMap и (c) ArrMap
--    Можно использовать любые стандартные функции.

class MapLike map where
    empty :: map k a
    lookup :: Ord k => k -> map k a -> Maybe a
    insert :: Ord k => k -> a -> map k a -> map k a
    delete :: Ord k => k -> map k a -> map k a
    fromList :: Ord k => [(k, a)] -> map k a

    fromList = foldl (\m (k', v) -> insert k' v m) empty


instance MapLike M.Map where
    empty = M.empty 
    lookup = M.lookup
    insert = M.insert
    delete = M.delete

newtype ListMap k v = ListMap [(k,v)]
instance MapLike ListMap where
    empty = ListMap []
    lookup key (ListMap lm) = L.lookup key lm
    insert key val (ListMap li) = ListMap $ go li
        where 
            go [] = [(key, val)]
            go ((k, v):xs) | k == key = (key, val) : xs
                           | otherwise = (k,v) : (go xs)
    
    delete key (ListMap li) = ListMap $ filter (\(k,v) -> k /= key) li

newtype ArrMap k v = ArrMap (k -> Maybe v)
instance MapLike ArrMap where
    empty = ArrMap (\_ -> Nothing)
    lookup key (ArrMap f) = f key 
    insert key val (ArrMap f) = ArrMap (\k -> if k == key then Just val else f k)
    delete key (ArrMap f) = ArrMap (\k -> if k == key then Nothing else f k)


-- 3. Написать instace Functor для (a) ListMap k и (b) ArrMap k.
instance Functor (ListMap k) where
    -- fmap :: (a -> b) -> ListMap k a -> ListMap k b
    fmap f (ListMap li) = ListMap $ map (\(k,v) -> (k, f v)) li

instance Functor (ArrMap k) where
    fmap f (ArrMap p) = ArrMap (\k -> maybe Nothing (\v -> Just $ f v) (p k))

