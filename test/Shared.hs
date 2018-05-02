module Shared
  ( t
  , s1, e1
  , s2, e2
  , s3, e3
  , s4, e4
  , s5, e5
  , s6, e6
  , s7, e7
  , s8, e8
  , s9, e9
  , s10, e10
  , s11, e11
  ) where

import Parser
import TypeInfer

t :: TypeEnv
t = Just $ VarMap [ (Var "x", TScheme [] (TCon "Int"))
                  , (Var "y", TScheme [TV 1] (TVar (TV 1)))
                  ]

-- 1
s1 :: String
s1 = "1"
e1 :: Expr
e1 = Lit (LInt 1)

-- x
s2 :: String
s2 = "x"
e2 :: Expr
e2 = Ref (Var "x")

-- x + 3
s3 :: String
s3 = "x + 3"
e3 :: Expr
e3 = Op Add (Ref (Var "x")) (Lit (LInt 3))

-- (x + 4) * y
s4 :: String
s4 = "(x + 4) * y"
e4 :: Expr
e4 = Op Mul (Paren (Op Add (Ref (Var "x")) (Lit (LInt 4)))) (Ref (Var "y"))

-- x == True
s5 :: String
s5 = "x == True"
e5 :: Expr
e5 = Op Eql (Ref (Var "x")) (Lit (LBool True))

-- (x + 6) == True
s6 :: String
s6 = "(x + 6) == True"
e6 :: Expr
e6 = Op Eql (Paren (Op Add (Ref (Var "x")) (Lit (LInt 6)))) (Lit (LBool True))

-- (x * 7, True)
s7 :: String
s7 = "(x * 7, True)"
e7 :: Expr
e7 = Pair [ Op Mul (Ref (Var "x")) (Lit (LInt 7))
          , (Lit (LBool True))
          ]

-- (x * 8, y)
s8 :: String
s8 = "(x * 8, y)"
e8 :: Expr
e8 = Pair [ Op Mul (Ref (Var "x")) (Lit (LInt 8))
          , (Ref (Var "y"))
          ]

-- [x * 9, y]
s9 :: String
s9 = "[x * 9, y]"
e9 :: Expr
e9 = List [ Op Mul (Ref (Var "x")) (Lit (LInt 9))
          , Ref (Var "y")
          ]

-- (x, y)
s10 :: String
s10 = "(x, y)"
e10 :: Expr
e10 = Pair [ Ref (Var "x")
           , Ref (Var "y")
          ]

-- let x = 11 in x
s11 :: String
s11 = "let x = 11 in x"
e11 :: Expr
e11 = Let (Var "x") (Lit (LInt 11)) (Ref (Var "x"))

{-
-- 1
e1 :: Expr
e1 = Lit (LInt 1)

-- x
e2 :: Expr
e2 = Ref (Var "x")

-- x + 3
e3 :: Expr
e3 = Op Add (Ref (Var "x")) (Lit (LInt 3))

-- (x + 4) * y
e4 :: Expr
e4 = Op Mul (Op Add (Ref (Var "x")) (Lit (LInt 4))) (Ref (Var "y"))

-- x == True
e5 :: Expr
e5 = Op Eql (Ref (Var "x")) (Lit (LBool True))

-- (x + 6) == True
e6 :: Expr
e6 = Op Eql (Op Add (Ref (Var "x")) (Lit (LInt 6))) (Lit (LBool True))

-- (x * 7, True)
e7 :: Expr
e7 = Pair [ Op Mul (Ref (Var "x")) (Lit (LInt 7))
          , (Lit (LBool True))
          ]

-- (x * 8, y)
e8 :: Expr
e8 = Pair [ Op Mul (Ref (Var "x")) (Lit (LInt 8))
          , (Ref (Var "y"))
          ]

-- [x * 9, y]
e9 :: Expr
e9 = List [ Op Mul (Ref (Var "x")) (Lit (LInt 9))
          , Ref (Var "y")
          ]

-- (x, y)
e10 :: Expr
e10 = Pair [ Ref (Var "x")
           , Ref (Var "y")
          ]
-}
