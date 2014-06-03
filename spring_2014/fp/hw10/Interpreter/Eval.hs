-- (1.5 балла)
module Eval
    ( Eval, runEval
    , Error, Store
    , update, getVar
    ) where

import qualified Data.Map as M
import Data.List
import Control.Monad

import Expr

type Error = String
type Store = M.Map String Value

newtype Eval a = Eval { runEval :: Store -> (Maybe a, [Error], Store) }

instance Monad Eval where
    return x = Eval (\s -> (Just x, [], s))
    Eval m >>= k = Eval go
        where 
            go s = case m s of
                (Just x, err, ns) -> add err (runEval (k x) ns)
                (Nothing, errl, st) -> (Nothing, errl, st)
            unpack (Eval x) = x
            add err (q, err2, s) = (q, err ++ err2, s)
    fail msg = Eval (\s -> (Nothing, [msg], s))

-- MonadPlus - аналог Alternative для монад
-- mzero - вычисление, которое ничего не делает, сразу завершается неуспехом
-- mplus m1 m2 пытается выполнить m1, если тот завершился неуспехом, выполняет m2
-- Примеры использования этого класса есть в Utils.hs
instance MonadPlus Eval where
    mzero = Eval (\s -> (Nothing, [], s))
    mplus (Eval l) (Eval r) = Eval go
        where
            go s = case l s of
                (Nothing, err, ns) -> add err (r ns)
                x -> x
            add err (q, err2, s) = (q, err ++ err2, s)

update :: String -> Value -> Eval ()
update k v = Eval (\s -> (Just (), [], M.insert k v s))

getVar :: String -> Eval Value
getVar k = Eval go
    where 
        go s = case M.lookup k s of
            Just v -> (Just v, [], s)
            Nothing -> (Nothing, ["Undefined variable " ++ k], s)
