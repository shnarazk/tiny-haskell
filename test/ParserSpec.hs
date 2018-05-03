module ParserSpec (spec) where
import Test.Hspec
import AST
import Parser
import Typing
import Cases

spec :: Spec
spec = do
  let run (n, s, a, _) =
        case parseHaskell s of
          Right a' -> it (show s ++ "\t => " ++ show a') $ a' `shouldBe` a
          Left a'  -> it ("\n" ++ a') $  NullExpr `shouldBe` a
  describe "Parse basic syntax" $ mapM_ run targets
  describe "Function Application" $ mapM_ run funApps
