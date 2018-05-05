module Lib (parsing, typing) where

import AST
import Parser
import Typing

-------------------------------------------------------------------------------- top-level interface
parsing :: String -> String
parsing str =
  case parseHaskell str of
    Left err  -> err
    Right exp -> show exp

typing :: String -> String
typing str =
  case parseHaskell str of
    Left err  -> err
    Right exp -> case inferExpr exp of
                   Right (t, e)  -> str ++ " :: " ++ prettyPrint t ++ " -- " ++ prettyPrint (shadow haskellEnv e)
                   Left e -> str ++ " => Error: " ++ prettyPrint e ++ "\n;;; The abstruct syntax tree of `" ++ str ++ "`\n" ++ show exp
