module Parser where

import Text.Parsec
import Text.Parsec.Char
import Lib

hVar :: Parsec String () Expr
hVar = do
  l <- letter
  s <- many (choice [letter, digit])
  return $ Ref (Var (l : s))

hLitInt :: Parsec String () Expr
hLitInt = do
  n <- many digit
  return $ Lit (LInt (read n))

hLitBool :: Parsec String () Expr
hLitBool = do
  b <- choice [string "True", string "False"]
  return $ Lit (LBool (read b))

hOp :: Parsec String () Expr
hOp = do
  l <- hExp <* spaces
  o <- choice [char '+', char '-', char '*'] <* spaces
  r <- hExp
  let op = case o of
             '+' -> Add
             '-' -> Sub
             '*' -> Mul
  return $ Op op l r

hExp :: Parsec String () Expr
hExp = choice [ hVar
              , hLitInt, hLitBool
              , hOp
              ]
