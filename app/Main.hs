module Main where

import AST
import Parser
import Typing

main :: IO ()
main = do
  str <- init <$> getContents
  case parseHaskell str of
    Left err  -> putStrLn err
    Right exp -> case runInfer exp of
                   Right (t, Just e) -> putStrLn $ str ++ " :: " ++ show t ++ " -- " ++ show e
                   Right (t, Nothing) -> putStrLn $ str ++ " :: " ++ show t ++ " -- empty env"
                   Left e -> putStrLn $ str ++ " => Error: " ++ show e ++ "\n;;; The abstruct syntax tree of '" ++ str ++ "'\n" ++ show exp

