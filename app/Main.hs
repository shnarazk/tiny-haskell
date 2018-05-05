module Main where

import AST
import Parser
import Typing

main :: IO ()
main = do
  str <- init <$> getContents
  case parseHaskell str of
    Left err  -> putStrLn err
    Right exp -> case inferExpr exp of
                   Right (t, e) -> putStrLn $ str ++ " :: " ++ show t ++ " -- " ++ show (shadow haskellEnv e)
                   Left e -> putStrLn $ str ++ " => Error: " ++ show e ++ "\n;;; The abstruct syntax tree of '" ++ str ++ "'\n" ++ show exp

