module LibSpec (spec) where
import Test.Hspec
import Lib

t :: TypeEnv
t = VarMap [ (Var "x", TScheme [] (TCon "Int"))
           , (Var "y", TScheme [TV "1"] (TVar (TV "1")))
           ]

-- (x + 3) * y
e1 :: Expr
e1 = Op Mul (Op Add (Ref (Var "x")) (Lit (LInt 3))) (Ref (Var "y"))

-- [x * 3, y]
e2 :: Expr
e2 = List [ Op Mul (Ref (Var "x")) (Lit (LInt 3))
          , Ref (Var "y")
          ]

spec :: Spec
spec = do
  describe "TypeEnv operations" $ do
    let y = Var "y"
    let res1 = tySubst t (TV "1", TCon "Int")
    it "subst t1 to Int" $ schemeOf res1 y `shouldBe` Just (TScheme [] (TCon "Int"))
    let res2 = tySubst t (TV "1", TVar (TV "2"))
    it "subst t1 to t2" $ schemeOf res2 y `shouldBe` Just (TScheme [] (TVar (TV "2")))
    let res3 = tySubst t (TV "1", TList (TVar (TV "1")))
    it "subst t1 to [t1]" $ schemeOf res3 y `shouldBe` Just (TScheme [] (TList (TVar (TV "1"))))
