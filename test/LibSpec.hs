module LibSpec (spec) where
import Test.Hspec
import Parser
import TypeInfer
import Shared

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
    let (Just emEn) = emptyEnv
    let infer' ex = case runInfer ex of
          Left _ -> (TVar (TV (-1)), emEn)
          Right (t, Just e) -> (t, e)
    let derivedBy e v = derived <$> schemeOf (Just e) v
    let (t1, s1) = infer' e1
    it (show e1 ++ " :: " ++ show t1 ++ " \t -- " ++ show s1)
      $ s1 `shouldBe` emEn
    let (t2, s2) = infer' e2
    it (show e2 ++ " :: " ++ show t2 ++ " \t -- " ++ show s2)
      $ derivedBy s2 x `shouldBe` Just (TVar (TV 1))
    let (t3, s3) = infer' e3
    it (show e3 ++ " :: "  ++ show t3 ++ " \t -- " ++ show s3)
      $ derivedBy s2 x `shouldBe` Just (TVar (TV 1))
    let (t4, s4) = infer' e4
    it (show e4 ++ " :: " ++ show t4 ++ " \t -- " ++ show s4)
      $ derivedBy s4 x `shouldBe` Just typeInt
    let (t5, s5) = infer' e5
    it (show e5 ++ " :: " ++ show t5 ++ " \t -- " ++ show s5)
      $ derivedBy s5 x `shouldBe` Just typeBool
    let (Left r6) = runInfer e6
    it (show e6 ++ " => " ++ show r6) $ r6 `shouldBe` UnificationFail e6 typeInt typeBool
    let (t7, s7) = infer' e7
    it (show e7 ++ " :: " ++ show t7 ++ " \t -- " ++ show s7)
      $ t7 `shouldBe` TPair [typeInt, typeBool]
    let (t8, s8) = infer' e8
    it (show e8 ++ " :: " ++ show t8 ++ " \t -- " ++ show s8)
      $ t8 `shouldBe` TPair [typeInt, TVar (TV 2)]
    let (t9, s9) = infer' e9
    it (show e9 ++ " :: " ++ show t9 ++ " \t -- " ++ show s9)
      $ t9 `shouldBe` TList typeInt
    let (t10, s10) = infer' e10
    it (show e10 ++ " :: " ++ show t10 ++ " \t -- " ++ show s10)
      $ t10 `shouldBe` TPair [TVar (TV 1), TVar (TV 2)]
