module ParserSpec (spec) where
import Test.Hspec
import AST
import Parser
import Typing
import Shared

spec :: Spec
spec = do
  let run (n, s, a, _) =
        case parseHaskell s of
          Right a' -> it (show s ++ "\t => " ++ show a') $ a' `shouldBe` a
          Left a'  -> it ("\n" ++ a') $ a `shouldBe` NullExpr
  describe "Parse with Parsec" $ mapM_ run targets
