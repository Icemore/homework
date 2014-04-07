import Test.HUnit
import qualified Data.Map as M

import Expr
import Nat
import Tree

-- Expr

expr = BinOp Minus (Var "y") (Const (I 3))
prog1 =
    [ "x" $= Const (B True)
    , "x" $= Const (I 3)
    , "y" $= Var "x" `plus` Var "x"
    , "x" $= Const (B False)
    ]
prog2 = 
    [ "x" $= Const (B True)
    , "x" $= Const (I 3)
    , "z" $= Const (B True)
    , "y" $= Var "x" `plus` Var "z"
    ]
prog3 =
    [ If (Var "x") [
        "a" $= Var "y" `plus` Const (I 3)
      ] `else_` [
        "b" $= Var "z" `plus` Const (I 3)
      ]
    , "c" $= Var "a"
    ]
fac =
    [ "i" $= Const (I 0)
    , "r" $= Const (I 1)
    , While (Var "i" `less` Var "n")
        [ "i" $= Var "i" `plus` Const (I 1)
        , "r" $= Var "r" `mul` Var "i"
        ]
    ]

testsExpr = [ evalExpr (M.fromList [("x", B False), ("y", I 5)]) expr ~?= Right (I 2)
            , errorsCount (evalExpr (M.fromList [("y", B False)]) expr) ~?= 1
            ]

testsProgram = [ evalProgram M.empty prog1 ~?= Right (M.fromList [("x",B False),("y",I 6)])
               , errorsCount (evalProgram M.empty prog2) ~?= 1
               , evalProgram (M.fromList [("n",I 5)]) fac ~?= Right (M.fromList [("n",I 5),("i",I 5),("r",I 120)])
               , evalProgram (M.fromList [("x",B True),("y",I 4),("z",B False)]) prog3 ~?= Right (M.fromList [("x",B True),("y",I 4),("z",B False),("a",I 7),("c",I 7)])
               , errorsCount (evalProgram (M.fromList [("x",B False),("y",I 4),("z",B False),("a",I 7)]) prog3) ~?= 1
               , evalProgram (M.fromList [("x",B True),("y",I 4),("z",I 5)]) prog3 ~?= Right (M.fromList [("x",B True),("y",I 4),("z",I 5),("a",I 7),("c",I 7)])
               , errorsCount (evalProgram (M.fromList [("x",B False),("y",I 4),("z",I 5)]) prog3) ~?= 1
               , errorsCount (evalProgram (M.fromList [("x",I 1),("y",I 4),("z",I 5),("a",I 6)]) prog3) ~?= 1
               ]

errorsCount = either length (const 0)

-- Tree

tree1 = Node "a" [Node "b" [Node "f" []], Node "c" [Node "d" []], Node "e" []]
tree2 = Node 1 [Node 2 [Node 4 [], Node 5 []], Node 3 []]
tree3 = Node "1" [Node "2" [Node "4" [], Node "5" []], Node "3" []]

testsTree =
    [ tree2 ~?= tree2
    , tree1 ~?/= tree3
    , show tree1 ~?= "\"a\":{\"b\":{\"f\"},\"c\":{\"d\"},\"e\"}"
    , show tree2 ~?= "1:{2:{4,5},3}"
    , fmap show tree2 ~?= tree3
    , (read "1:{2:{4,5},3}" :: Tree Int) ~?= tree2
    , (reads "1:{2:{4,5},}" :: [(Tree Int,String)]) ~?= [(Node 1 [],":{2:{4,5},}")]
    , (reads ",1:{2:{4,5},}" :: [(Tree Int,String)]) ~?= []
    ]

-- Nat

one = Suc Zero
two = Suc one
three = Suc two

testsNat =
    [ two ~?= two
    , three ~?/= two
    , (two < three) ~?= True
    , (one > three) ~?= False
    , show two ~?= "2"
    , show three ~?= "3"
    , fromInteger 3 ~?= three
    , three + fromInteger 7 ~?= fromInteger 10
    , three * fromInteger 7 ~?= fromInteger 21
    ]

-- main

(~?/=) :: (Eq a, Show a) => a -> a -> Test
x ~?/= y = TestCase $ assertBool (show x ++ " shoud not be equal to " ++ show y) (x /= y)

main = fmap (\_ -> ()) $ runTestTT $ test $
       label "evalExpr" testsExpr
    ++ label "evalProgram" testsProgram
    ++ label "Tree" testsTree
    ++ label "Nat" testsNat
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
