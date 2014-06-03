-- (0.5 балла)
import Counter

-- Эти две функции отличаются от обычных тем, что, вызывая tick, они считают сколько шагов заняло их выполнение.
filter' :: (a -> Bool) -> [a] -> Counter [a]
filter' p (x:xs) = do
    ys <- filter' p xs
    tick
    if (p x) then 
        return (x:ys)
    else
        return ys
filter' _ [] = return []

append :: [a] -> [a] -> Counter [a]
append (x:xs) ys = do
    r <- append xs ys
    tick
    return (x:r)
append [] ys = return ys

-- Реализуйте qsort через filter' и append
qsort :: Ord a => [a] -> Counter [a]
qsort (x:xs) = do
    l <- filter' (<=x) xs >>= qsort
    r <- filter' (>x) xs >>= qsort
    append [x] r >>= append l
qsort [] = return []

-- Первый вызов должен занимать большее количество тиков ~ в 2 раза
main = do
    print $ runCounter $ qsort [1..15]
    print $ runCounter $ qsort [8,4,12,2,6,10,14,1,3,5,7,9,11,13,15]
