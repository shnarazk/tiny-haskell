module ParserSpec (spec) where
import Test.Hspec
import Parser
import TypeInfer
import Shared

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
    let a11 = runHaskell s11
    it (show s11 ++ " => " ++ show a11) $ a11 `shouldBe` Right e11
