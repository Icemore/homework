module Utils
    ( try
    , liftM2'
    , liftM3'
    , liftM4'
    , liftM5'
    , ap'
    ) where

import Control.Monad

-- try m выполняет m и возвращает Just x, если m вернуло x; если m завершилось неуспешно, возвращает Nothing
-- Аналог optional из Control.Applicative
try :: MonadPlus m => m a -> m (Maybe a)
try m = liftM Just m `mplus` return Nothing

-- Аналог liftM2 из Control.Monad, только выполняет оба действия, даже если первое завершилось неуспехом
liftM2' :: MonadPlus m => (a -> b -> c) -> m a -> m b -> m c
liftM2' f ma mb = do
    a' <- try ma
    b <- mb
    maybe mzero (\a -> return $ f a b) a'

liftM3' :: MonadPlus m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3' f ma mb mc = do
    a' <- try ma
    b' <- try mb
    c <- mc
    maybe mzero return $ do
        a <- a'
        b <- b'
        return (f a b c)

liftM4' :: MonadPlus m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
liftM4' f ma mb mc md = do
    a' <- try ma
    b' <- try mb
    c' <- try mc
    d <- md
    maybe mzero return $ do
        a <- a'
        b <- b'
        c <- c'
        return (f a b c d)

liftM5' :: MonadPlus m => (a -> b -> c -> d -> e -> f) -> m a -> m b -> m c -> m d -> m e -> m f
liftM5' f ma mb mc md me = do
    a' <- try ma
    b' <- try mb
    c' <- try mc
    d' <- try md
    e <- me
    maybe mzero return $ do
        a <- a'
        b <- b'
        c <- c'
        d <- d'
        return (f a b c d e)

ap' :: MonadPlus m => m (a -> b) -> m a -> m b
ap' = liftM2' ($)
