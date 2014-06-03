-- (1.5 балла)
{-# LANGUAGE TupleSections #-}

import Control.Monad
import qualified Data.Map as M
import Test.HUnit
import Data.Maybe(fromMaybe)

import Expr
import Eval
import Utils

getInt :: Eval Value -> Eval Integer
getInt m = m >>= go
    where
        go (I i) = do return i
        go (B b) = do fail "Int expected"

getBool :: Eval Value -> Eval Bool
getBool m = m >>= go
    where
        go (I i) = do fail "Bool expected"
        go (B b) = do return b

if' :: Eval Value -> Eval () -> Maybe (Eval ()) -> Eval ()
if' c t e = do
    cond <- getBool c
    if cond then do t 
    else do fromMaybe (return ()) e

evalExpr :: Expr -> Eval Value
evalExpr (Const v) = return v
evalExpr (Var str) = getVar str
evalExpr (UnOp Neg expr) = do
    i <- getInt $ evalExpr expr
    return $ I $ negate i
evalExpr (UnOp Not expr) = do
    b <- getBool $ evalExpr expr
    return $ B $ not b

evalExpr (BinOp op le re) = evalBinOp op (evalExpr le) (evalExpr re)

evalBinOp :: BinOp -> Eval Value -> Eval Value -> Eval Value
evalBinOp Plus l r = iToVal $ liftM2 (+) (getInt l) (getInt r)
evalBinOp Mul l r = iToVal $ liftM2 (*) (getInt l) (getInt r)
evalBinOp Minus l r = iToVal $ liftM2 (-) (getInt l) (getInt r)
evalBinOp And l r = bToVal $ liftM2 (&&) (getBool l) (getBool r)
evalBinOp Or l r = bToVal $ (liftM2' (||) (getBool l) (getBool r) `mplus` return False)
evalBinOp Less l r = bToVal $ liftM2 (<) (getInt l) (getInt r)
evalBinOp Greater l r = bToVal $ liftM2 (>) (getInt l) (getInt r)
evalBinOp Equals l r = bToVal $ liftM2 (==) (getInt l) (getInt r)

iToVal e = liftM I e
bToVal e = liftM B e

evalStatement :: Statement -> Eval ()
evalStatement (If c t e) = if' (evalExpr c) (evalProgram t) (liftM evalProgram e)
evalStatement (While c b) = if' (evalExpr c) t Nothing
    where 
        t = do
            evalProgram b
            evalStatement (While c b)
    
evalStatement (Assign name expr) = do
    val <- evalExpr expr
    update name val

evalProgram :: Program -> Eval ()
evalProgram = mapM_ evalStatement

------------------------------------------------------------------------------------------------
-- tests
------------------------------------------------------------------------------------------------

test1 = not_ (Var "x") ||| Var "y" <<< Const (I 3) &&& Var "z" === Var "y" &&&
    Const (I 5) <<< Var "y" +++ Const (I 7) *** Var "z" +++ Var "y" *** Const (I 3)

test2 = neg (Const $ I 5) +++ neg (Const $ I 3) *** Const (I 2) -.- Const (I 7)

test3 =
    [ "r" $= Const (I 1)
    , While (Var "n" >>> Const (I 0))
        [ "r" $= Var "r" *** Var "n"
        , "n" $= Var "n" -.- Const (I 1)
        ]
    ]

main = fmap (\_ -> ()) $ runTestTT $ test
    [ errorsCount (runEval (evalExpr test1) $ M.fromList [("x",I 3),("y",I 5),("f",I 5)]) ~?= 2
        -- 2 ошибки: неизвестная переменная z и несоответствие типов (в том месте, где вычисляется "!x")
    , let m = M.fromList [("x",B True),("y",I 5),("z",I 5)] in runEval (evalExpr test1) m ~?= (Just (B False), [], m)
    , let m = M.fromList [("x",B True),("y",I 2),("z",I 2)] in runEval (evalExpr test1) m ~?= (Just (B True ), [], m)
    , runEval (evalExpr test2) M.empty ~?= (Just (I (-18)), [], M.empty)
    , runEval (evalProgram test3) (M.fromList [("n",I 5)]) ~?= (Just (), [], M.fromList [("n",I 0),("r",I 120)])
    ]
  where
    errorsCount (_,es,_) = length es
