module LibSpec (spec) where
import Test.Hspec
import Lib

t :: TypeEnv
t = VarMap [ (Var "x", TScheme [] (TCon "Int"))
           , (Var "y", TScheme [TV 1] (TVar (TV 1)))
           ]

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

-- [x * 5, y]
e5 :: Expr
e5 = List [ Op Mul (Ref (Var "x")) (Lit (LInt 5))
          , Ref (Var "y")
          ]

spec :: Spec
spec = do
  describe "TypeEnv operations" $ do
    let y = Var "y"
    let res1 = tySubst t (TV 1, TCon "Int")
    it "subst t1 to Int" $ schemeOf res1 y `shouldBe` Just (TScheme [] (TCon "Int"))
    let res2 = tySubst t (TV 1, TVar (TV 2))
    it "subst t1 to t2" $ schemeOf res2 y `shouldBe` Just (TScheme [] (TVar (TV 2)))
    let res3 = tySubst t (TV 1, TList (TVar (TV 1)))
    it "subst t1 to [t1]" $ schemeOf res3 y `shouldBe` Just (TScheme [] (TList (TVar (TV 1))))
  describe "Infer" $ do
    let x = Var "x"
    let infer' e = case runInfer e of
          Left _ -> (TVar (TV (-1)), emptyEnv)
          Right e -> e
    let derivedBy e v = derived <$> schemeOf e v
    let (t1, s1) = infer' e1
    it (show e1 ++ " => " ++ show t1) $ s1 `shouldBe` VarMap []
    let (t2, s2) = infer' e2
    it (show e2 ++ " => " ++ show t2) $ derivedBy s2 x `shouldBe` Just (TVar (TV 1))
    let (t3, s3) = infer' e3
    it (show e3 ++ " => " ++ show t3) $ derivedBy s3 x `shouldBe` Just typeInt
    let (t4, s4) = infer' e4
    it (show e4 ++ " => " ++ show t4) $ derivedBy s4 x `shouldBe` Just typeInt
