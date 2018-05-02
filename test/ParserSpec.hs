module ParserSpec (spec) where
import Test.Hspec
import Lib
import Parser

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

spec :: Spec
spec = do
  describe "Parse with Parsec" $ do
    let a1 = runHaskell s1
    it (show s1 ++ " => " ++ show a1) $ a1 `shouldBe` Right e1
    let a2 = runHaskell s2
    it (show s2 ++ " => " ++ show a2) $ a2 `shouldBe` Right e2
    let a3 = runHaskell s3
    it (show s3 ++ " => " ++ show a1) $ a3 `shouldBe` Right e3
    let a4 = runHaskell s4
    it (show s4 ++ " => " ++ show a4) $ a4 `shouldBe` Right e4
    let a5 = runHaskell s5
    it (show s5 ++ " => " ++ show a5) $ a5 `shouldBe` Right e5
    let a6 = runHaskell s6
    it (show s6 ++ " => " ++ show a6) $ a6 `shouldBe` Right e6
    let a7 = runHaskell s7
    it (show s7 ++ " => " ++ show a7) $ a7 `shouldBe` Right e7
    let a8 = runHaskell s8
    it (show s8 ++ " => " ++ show a8) $ a8 `shouldBe` Right e8
    let a9 = runHaskell s9
    it (show s9 ++ " => " ++ show a9) $ a9 `shouldBe` Right e9
    let a10 = runHaskell s10
    it (show s10 ++ " => " ++ show a10) $ a10 `shouldBe` Right e10
