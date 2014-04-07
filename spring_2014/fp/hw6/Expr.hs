module Expr where

import qualified Data.Map as M

-- (3 балла)

data Value = I Integer | B Bool deriving (Eq, Show)
data BinOp = Plus | Mul | Minus | Less | Greater | Equals
data UnOp = Neg | Not
data Expr = BinOp BinOp Expr Expr | UnOp UnOp Expr | Const Value | Var String
data Statement = Assign String Expr | If Expr Program (Maybe Program) | While Expr Program
type Program = [Statement]

infixr 0 $=
($=) = Assign

infix 4 `less`, `greater`
less = BinOp Less
greater = BinOp Greater

infixl 7 `mul`
mul = BinOp Mul

infixl 6 `plus`, `minus`
plus = BinOp Plus
minus = BinOp Minus

infix 1 `else_`
else_ :: (Maybe Program -> Statement) -> Program -> Statement
else_ f e = f (Just e)

type Error = String

evalBinOp :: BinOp -> Value -> Value -> Either [Error] Value
evalBinOp _ (I _) (B _) = Left ["wrong types for binary operation"]
evalBinOp _ (B _) (I _) = Left ["wrong types for binary operation"]
evalBinOp op (I left) (I right) = case op of
    Plus -> getI (+)
    Minus -> getI (-)
    Mul -> getI (*)
    Less -> getB (<)
    Greater -> getB (>)
    Equals -> getB (==)
    where 
        getI p = Right $ I $ p left right
        getB p = Right $ B $ p left right

evalBinOp Equals (B left) (B right) = Right $ B $ left == right
evalBinOp _ (B _) (B _) = Left ["unsupported operation for Bool"]

evalUnOp :: UnOp -> Value -> Either [Error] Value
evalUnOp Neg (I x) = Right $ I $ -x
evalUnOp Neg (B _) = Left ["negation of Bool"]
evalUnOp Not (B b) = Right $ B $ not b
evalUnOp Not (I _) = Left ["calling not from Integer"]

-- evalExpr m e интерпретирует e в контексте m, который сопоставлят имени переменной ее значение.
-- evalExpr возвращает либо успешно вычисленный результат, либо список ошибок.
-- Ошибки бывают двух видов: необъявленная переменная и несоответствие типов.
evalExpr :: M.Map String Value -> Expr -> Either [Error] Value
evalExpr m (BinOp op leftExpr rightExpr) = 
    let
        leftVal = evalExpr m leftExpr
        rightVal = evalExpr m rightExpr
    in case (leftVal, rightVal) of
        (Left xs, Left ys) -> Left (xs ++ ys)
        (Left xs, _) -> Left xs
        (_, Left ys) -> Left ys
        (Right v1, Right v2) -> evalBinOp op v1 v2

evalExpr m (UnOp op expr) = 
    let 
        val = evalExpr m expr
    in case val of
        Left xs -> Left xs
        Right v -> evalUnOp op v

evalExpr m (Const v) = Right v
evalExpr m (Var str) = 
    let val = M.lookup str m
    in case val of
        Nothing -> Left ["undefined var: " ++ str]
        Just v -> Right v
        
evalIf :: M.Map String Value -> Value -> Program -> Maybe Program -> Either [Error] (M.Map String Value)
evalIf m (I _) _ _ = Left ["int in if condition"]
evalIf m (B True) prog _ = evalProgram m prog
evalIf m (B False) _ (Just prog) = evalProgram m prog
evalIf m (B False) _ Nothing = Right m

-- evalStatement m e интерпретирует e в контексте m.
-- evalStatement возвращает либо обновленный контекст, либо список ошибок.
evalStatement :: M.Map String Value -> Statement -> Either [Error] (M.Map String Value)
evalStatement m (Assign str expr) =
    let val = evalExpr m expr
    in case val of
        Left xs -> Left xs
        Right v -> Right $ M.insert str v m

evalStatement m (If expr prog el) =
    let val = evalExpr m expr
    in case val of
        Left xs -> Left xs
        Right v -> evalIf m v prog el

evalStatement m (While expr prog) =
    let val = evalExpr m expr
    in case val of
        Left xs -> Left xs
        Right (B False) -> Right m
        Right (B True) -> 
            let body = evalProgram m prog 
            in case body of
                Left xs -> Left xs
                Right newM -> evalStatement newM (While expr prog)

evalProgram :: M.Map String Value -> Program -> Either [Error] (M.Map String Value)
evalProgram m (s:sl) = 
    let val = evalStatement m s
    in case val of
        Left xs -> Left xs
        Right newM -> evalProgram newM sl
evalProgram m [] = Right m
