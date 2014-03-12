-- 1. fib n вовзращает n-ое число Фибоначчи
--    (1 балл)
fib :: Integer -> Integer
fib n = helper n 1 0
    where 
        helper 0 a b = b
        helper n a b = helper (n - 1) (a + b) a

-- 2a. Написать функцию, возвращающую количество цифр числа.
--     Для целочисленного деления можете использовать функции div и mod.
--    (0.5 балла)
numberOfDigits :: Integer -> Integer
numberOfDigits n | n < 10    = 1
                 | otherwise = 1 + (numberOfDigits $ n `div` 10)

-- 2b. Написать функцию, возвращающую сумму цифр числа.
--    (0.5 балла)
sumOfDigits :: Integer -> Integer
sumOfDigits n | n < 10    = n
              | otherwise = (n `mod` 10) + (sumOfDigits $ n `div` 10)

-- 3. gcd' возвращает НОД.
--    (1 балл)
gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = gcd b (a `mod` b)

-- 4. minp p возвращает минимальное по модулю число x такое, что p x == True. Если такого x не существует, minp не завершается.
--    (1 балл)
minp :: (Integer -> Bool) -> Integer
minp  p = helper 0 
    where 
        helper x | p x = x
                 | otherwise = helper (x + 1)

-- 5. integral f a b возвращает значение определенного интеграла функции f на отрезке [a,b].
--    Для реализации можете использовать метод трапеций.
--    (2 балла)
integral :: (Double -> Double) -> Double -> Double -> Double
integral f a b =  ((helper cnt (a + step)) + 0.5 * ((f a) + (f b))) * step
    where
        step = 1e-3
        cnt = (b - a) / step - 1
        helper 0 x = 0
        helper n x = (f x) + (helper (n-1) (x+step))

-- 6. Реализуйте оператор примитивной рекурсии rec, используя функцию (-), укажите тип rec.
--    (1 балл)
rec :: a -> (Integer -> a -> a) -> Integer -> a
rec f g 0 = f
rec f g n = g (n - 1) (rec f g (n - 1))

-- 7. Реализуйте факториал при помощи rec.
--    (1 балл)
facRec :: Integer -> Integer
facRec = rec 1 (\ x y -> (x + 1) * y) 

-- 8. Реализуйте факториал при помощи fix.
--    (1 балл)
facFix :: Integer -> Integer
facFix = fix (\f n -> if (n == 1) then 1 else n * (f $ n - 1))
  where fix f = f (fix f)
