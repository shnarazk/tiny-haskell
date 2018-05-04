module Cases ( tenv, errors, infinites, targets, funApps ) where

import AST
import Typing

tenv :: TypeEnv
tenv = Just $ VarMap [ (Var "x", TScheme [] (TCon "Int"))
                  , (Var "y", TScheme [TV 1] (TVar (TV 1)))
                  ]

type Instance = (Int, String, Expr, Either TypeError Type)

errors :: [Instance]
errors = zipWith (\i (s, a, t) -> (i, s, a, t)) [1..] errors'
errors' =
  [
   ( "*"
    , NullExpr
    , Right TInt
    )
  ]

infinites :: [Instance]
infinites = zipWith (\i (s, a, t) -> (i, s, a, t)) [1..] infinites'
infinites' =
  [
    ( "f f == f"
    , Op Eql (App [Ref (Var "f"),Ref (Var "f")]) (Ref (Var "f"))
    , Left (InfiniteType (Ref (Var "f")) (TV 1) (TArr [TVar (TV 1), TVar (TV 2)]))
    )
  , ( "f == f f"
    , Op Eql (Ref (Var "f")) (App [Ref (Var "f"), Ref (Var "f")])
    , Left (InfiniteType (Ref (Var "f")) (TV 1) (TArr [TVar (TV 1), TVar (TV 2)]))
    )
  ]

targets :: [Instance]
targets = zipWith (\i (s, a, t) -> (i, s, a, t)) [1..] targets'

targets' =
  [ ( "1"
    , Lit (LInt 1)
    , Right TInt
    )
  , ( "x + 2"
    , Op Add (Ref (Var "x")) (Lit (LInt 2))
    , Right TInt
    )
  , ( "x * 2"
    , Op Mul (Ref (Var "x")) (Lit (LInt 2))
    , Right TInt
    )
  , ( "(x + 3) * y"
    , Op Mul (Paren (Op Add (Ref (Var "x")) (Lit (LInt 3)))) (Ref (Var "y"))
    , Right TInt
    )
  , ( "x == True"
    , Op Eql (Ref (Var "x")) (Lit (LBool True))
    , Right TBool
    )
  , ( "(x + 5) == True"
    , Op Eql (Paren (Op Add (Ref (Var "x")) (Lit (LInt 5)))) (Lit (LBool True))
    , Left (UnificationFail NullExpr TInt TBool)
    )
  , ( "(x * 6, True)"
    , Pair [ Op Mul (Ref (Var "x")) (Lit (LInt 6)), (Lit (LBool True))]
    , Right (TTpl [TInt, TBool])
    )
  , ( "(x * 7, y)"
    , Pair [ Op Mul (Ref (Var "x")) (Lit (LInt 7)), (Ref (Var "y"))]
    , Right (TTpl [TInt, TVar (TV 2)])
    )
  , ( "[x * 8, y]"
    , List [ Op Mul (Ref (Var "x")) (Lit (LInt 8)), Ref (Var "y")]
    , Right (TLst TInt)
    )
  , ( "(x, y)"
    , Pair [ Ref (Var "x") , Ref (Var "y")]
    , Right (TTpl [TVar (TV 1), TVar (TV 2)])
    )
  , ( "let x = 10 in x"
    , Let (Var "x") (Lit (LInt 10)) (Ref (Var "x"))
    , Right TInt
    )
  , ( "let x = y in x + 11"
    , Let (Var "x") (Ref (Var "y")) (Op Add (Ref (Var "x")) (Lit (LInt 11)))
    , Right TInt
    )
  ]

funApps :: [Instance]
funApps = zipWith (\i (s, a, t) -> (i, s, a, t)) [1..] funApps'
funApps' =
  [
    ( "f 1"
    , App [Ref (Var "f"),  Lit (LInt 1)]
    , Right (TVar (TV 2))
    )
  , ( "f 1 True"
    , App [Ref (Var "f"),  Lit (LInt 1), Lit (LBool True)]
    , Right (TVar (TV 2))
    )
  , ( "x + 3"
    , Op Add (Ref (Var "x")) (Lit (LInt 3))
    , Right TInt
    )
  , ( "x * 4"
    , Op Mul (Ref (Var "x")) (Lit (LInt 4))
    , Right TInt
    )
  , ( "x 5 * y 5"
    , Op Mul (App [Ref (Var "x"), Lit (LInt 5)]) (App [Ref (Var "y"), Lit (LInt 5)])
    , Right TInt
    )
  , ( "x 6 * x 60"
    , Op Mul (App [Ref (Var "x"), Lit (LInt 6)]) (App [Ref (Var "x"), Lit (LInt 60)])
    , Right TInt
    )
--  , ( "x 7 * x True"
--    , Op Mul (App [Ref (Var "x"), Lit (LInt 7)]) (App [Ref (Var "x"), Lit (LBool True)])
--    , Left (UnificationFail NullExpr (TArr [TInt, TInt]) (TArr [TBool, TVar (TV 3)]))
--    )
  ]
