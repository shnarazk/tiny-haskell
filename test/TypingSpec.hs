module TypingSpec (spec) where
import Test.Hspec
import AST
import Typing
import Shared

spec :: Spec
spec = do
  describe "TypeEnv operations" $ do
    let y = Var "y"
    let res1 = subst t [(TVar (TV 1), TCon "Int")]
    it "subst t1 to Int" $ schemeOf res1 y `shouldBe` Just (TScheme [] (TCon "Int"))
    let res2 = subst t [(TVar (TV 1), TVar (TV 2))]
    it "subst t1 to t2" $ schemeOf res2 y `shouldBe` Just (TScheme [] (TVar (TV 2)))
    let res3 = subst t [(TVar (TV 1), TLst (TVar (TV 1)))]
    it "subst t1 to [t1]" $ schemeOf res3 y `shouldBe` Just (TScheme [] (TLst (TVar (TV 1))))
--  let x = Var "x"
--  let derivedBy e v = derived <$> schemeOf (Just e) v
  let run (n, s, NullExpr, _) = it (show n ++ " passed") $ NullExpr `shouldBe` NullExpr
      run (n, s, a, r) = case (runInfer a, r) of
        (Left e', Left e)              -> it (s ++ " => " ++ show e')
          $ case (e', e) of
           (UnificationFail _ p1 p2, UnificationFail _ q1 q2)  -> (p1, p2) `shouldBe` (q1, q2)
           _ -> e' `shouldBe` e
        (Left e', _)                   -> it (s ++ " => " ++ show e') $ Left e' `shouldBe` r
        (Right (t', Just e'), Right t) -> it (s ++ " :: " ++ show t' ++ " \t -- " ++ show e') $ t' `shouldBe` t
        (Right (t', Just e'), _)       -> it (s ++ " :: " ++ show t' ++ " \t -- " ++ show e') $ Right t' `shouldBe` r
  describe "Infer" $ mapM_ run targets
