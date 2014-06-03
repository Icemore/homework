-- (1 балл)
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StateIO
    ( StateIO
    , runStateIO, execStateIO, evalStateIO
    ) where

import Control.Monad.State
import Data.IORef

newtype StateIO s a = StateIO { getStateIO :: IORef s -> IO a }

instance Monad (StateIO s) where
    return x = StateIO (\s -> return x)
    StateIO f >>= k = StateIO go
        where 
            go s = do
                x <- f s
                (unpack $ k x) s
            unpack (StateIO f) = f
instance MonadState s (StateIO s) where
    get = StateIO readIORef
    put v = StateIO (\s -> writeIORef s v)

runStateIO :: StateIO s a -> s -> IO (a,s)
runStateIO (StateIO f) s = do
    r <- newIORef s
    x <- f r
    t <- readIORef r
    return (x, t)

execStateIO :: StateIO s a -> s -> IO s
execStateIO st s = runStateIO st s >>= return . snd

evalStateIO :: StateIO s a -> s -> IO a
evalStateIO st s = runStateIO st s >>= return . fst
