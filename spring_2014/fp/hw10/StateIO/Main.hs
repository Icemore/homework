-- (0.5 балла)
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Test.HUnit

import StateIO

-- Эти instance можно реализовать, используя реализацию MonadState
instance MonadReader s (StateIO s) where
    ask = get
    local f st = st >>= go
        where
            go s = do
                x <- get
                put (f x)
                return s

instance Monoid s => MonadWriter s (StateIO s) where
    tell w = do
        x <- get
        put $ mappend x w
    listen m = m >>= go
        where
            go s = do
                x <- get
                return (s, x)
    pass m = m >>= go
        where
            go (s, f) = do
                x <- get 
                put $ f x
                return s

-- tests

reverse' :: [a] -> StateIO [a] [a]
reverse' xs = put [] >> rev xs >> get
  where
    rev :: [a] -> StateIO [a] ()
    rev [] = return ()
    rev (x:xs) = modify (x:) >> rev xs

testReader :: StateIO Int Int
testReader = do
    x <- ask
    y <- local (+1) ask
    z <- ask
    return (x + y + z)

testWriter :: Int -> StateIO String ()
testWriter 0 = tell "0" >> return ()
testWriter n = do
    tell (show n ++ " ")
    testWriter (n - 1)
    tell (" " ++ show n)

main = fmap (const ()) $ runTestTT $ test
    $    label "MonadState"
    [ TestCase $ liftM (== reverse [1..100]) (evalStateIO (reverse' [1..100]) undefined)
        @? "reverse' [1..100] shoud be equal to [100..1]"
    ] ++ label "MonadReader"
    [ TestCase $ let p = 7; r = 3 * p + 1 in liftM (== r) (evalStateIO testReader p)
        @? "testReader " ++ show p ++ " shoud be equal to " ++ show r
    ] ++ label "MonadWriter"
    [ TestCase $ let p = 3; r = "3 2 1 0 1 2 3" in liftM (== r) (evalStateIO (testWriter p >> get) "")
        @? "testWriter " ++ show p ++ " shoud be equal to " ++ show r
    ]
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
