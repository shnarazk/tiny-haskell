module Main where

import AST
import Parser
import Typing

main :: IO ()
main = do
  str <- getContents
  case parseHaskell str of
    Left err  -> putStrLn err
    Right exp -> case runInfer exp of
                      Right (t, Just e) -> putStrLn $ show exp ++ " :: " ++ show t ++ " -- " ++ show e
                      Right (t, Nothing) -> putStrLn $ show exp ++ " :: " ++ show t ++ " -- empty env"
                      Left e -> putStrLn $ show exp ++ " => Error: " ++ show e
