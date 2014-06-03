-- (0.5 балла)
module Counter
    ( Counter
    , tick
    , runCounter
    ) where

-- Монада Counter считает количество тиков, т.е. вызовов функции tick
data Counter a = Counter Int a

-- Возвращает результат вычислений и количество тиков
runCounter :: Counter a -> (a, Int)
runCounter (Counter cnt val) = (val, cnt)

instance Monad Counter where
    return val = Counter 0 val
    (Counter cnt1 v) >>= k = case k v of (Counter cnt2 res) -> Counter (cnt1+cnt2) res

tick :: Counter ()
tick = Counter 1 ()
