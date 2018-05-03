module Shared ( t, targets ) where

import AST
import Typing

t :: TypeEnv
t = Just $ VarMap [ (Var "x", TScheme [] (TCon "Int"))
                  , (Var "y", TScheme [TV 1] (TVar (TV 1)))
                  ]

type Instance = (Int, String, Expr, Either TypeError Type)

targets :: [Instance]
targets = zipWith (\i (s, a, t) -> (i, s, a, t)) [1..] targets'

targets' =
  [ ( "1"
    , Lit (LInt 1)
    , Right TInt
    )
  , ( "*"
    , NullExpr
    , Right TInt
    )
  , ( "x"
    , Ref (Var "x")
    , Right (TVar (TV 1))
    )
  , ( "x + 3"
    , Op Add (Ref (Var "x")) (Lit (LInt 3))
    , Right TInt
    )
  , ( "(x + 4) * y"
    , Op Mul (Paren (Op Add (Ref (Var "x")) (Lit (LInt 4)))) (Ref (Var "y"))
    , Right TInt
    )
  , ( "x == True"
    , Op Eql (Ref (Var "x")) (Lit (LBool True))
    , Right TBool
    )
  , ( "(x + 6) == True"
    , Op Eql (Paren (Op Add (Ref (Var "x")) (Lit (LInt 6)))) (Lit (LBool True))
    , Left (UnificationFail NullExpr TInt TBool)
    )
  , ( "(x * 7, True)"
    , Pair [ Op Mul (Ref (Var "x")) (Lit (LInt 7)), (Lit (LBool True))]
    , Right (TTpl [TInt, TBool])
    )
  , ( "(x * 8, y)"
    , Pair [ Op Mul (Ref (Var "x")) (Lit (LInt 8)), (Ref (Var "y"))]
    , Right (TTpl [TInt, TVar (TV 2)])
    )
  , ( "[x * 9, y]"
    , List [ Op Mul (Ref (Var "x")) (Lit (LInt 9)), Ref (Var "y")]
    , Right (TLst TInt)
    )
  , ( "(x, y)"
    , Pair [ Ref (Var "x") , Ref (Var "y")]
    , Right (TTpl [TVar (TV 1), TVar (TV 2)])
    )
  , ( "let x = 11 in x"
    , Let (Var "x") (Lit (LInt 11)) (Ref (Var "x"))
    , Right TInt
    )
  , ( "let x = y in x + 12"
    , Let (Var "x") (Ref (Var "y")) (Op Add (Ref (Var "x")) (Lit (LInt 12)))
    , Right TInt
    )
  ]
