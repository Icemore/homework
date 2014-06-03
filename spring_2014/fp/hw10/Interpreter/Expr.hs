module Expr
    ( Value(..), Expr(..), UnOp(..), BinOp(..), Statement(..), Program
    , ($=), (<<<), (>>>), (***), (+++), (-.-), (|||), (&&&), (===), not_, neg
    , priority
    ) where

import Text.PrettyPrint

data Value = I Integer | B Bool deriving Eq

data BinOp = Plus | Mul | Minus | And | Or | Less | Greater | Equals deriving Eq

data UnOp = Neg | Not deriving Eq

data Expr
    = Const Value
    | Var String
    | BinOp BinOp Expr Expr
    | UnOp UnOp Expr
    deriving Eq

type Program = [Statement]

data Statement
    = If Expr Program (Maybe Program)
    | While Expr Program
    | Assign String Expr
    deriving Eq

infixr 0 $=
($=) = Assign

infix 5 <<<, >>>, ===
(<<<) = BinOp Less
(>>>) = BinOp Greater
(===) = BinOp Equals

infixl 7 ***
(***) = BinOp Mul

infixl 6 +++, -.-
(+++) = BinOp Plus
(-.-) = BinOp Minus

infixl 4 |||, &&&
(|||) = BinOp Or
(&&&) = BinOp And

infix 1 `else_`
else_ :: (Maybe Program -> Statement) -> Program -> Statement
else_ f e = f (Just e)

not_ :: Expr -> Expr
not_ = UnOp Not

neg :: Expr -> Expr
neg = UnOp Neg

instance Show Value where
    show (I v) = show v
    show (B True) = "true"
    show (B False) = "false"

instance Show BinOp where
    show Plus = " + "
    show Mul = " * "
    show Minus = " - "
    show And = " && "
    show Or = " || "
    show Less = " < "
    show Greater = " > "
    show Equals = " == "

instance Show UnOp where
    show Neg = "-"
    show Not = "!"

priority And = 4
priority Or = 4
priority Less = 5
priority Greater = 5
priority Equals = 5
priority Plus = 6
priority Minus = 6
priority Mul = 7

instance Show Expr where
    showsPrec _ (Const v) = shows v
    showsPrec _ (Var x) = showString x
    showsPrec p (BinOp op e1 e2) = showParen (p > priority op) $
        showsPrec (priority op) e1 . shows op . showsPrec (priority op + 1) e2
    showsPrec p (UnOp op e) = shows op . showsPrec 10 e

instance Show Statement where
    show = render . pretty
      where
        curly p b = vcat [p <+> lbrace, nest 4 b, rbrace]
        if_then c t = curly (text "if" <+> parens (text (show c))) (vcat $ map pretty t)
        pretty (If c t Nothing) = if_then c t
        pretty (If c t (Just e)) = curly (if_then c t <+> text "else") (vcat $ map pretty e)
        pretty (While c b) = curly (text "while" <+> parens (text (show c))) (vcat $ map pretty b)
        pretty (Assign v e) = text v <+> equals <+> text (show e) <> semi
