module Parser ( runHaskell
              ) where

import Text.Parsec
import Text.Parsec.Char
import Lib

runHaskell :: String -> Either String Expr
runHaskell str = case parse hLine "ERROR" str of
                   Left err -> let -- l = lines str !! (sourceLine (errorPos err) - 2)
                                   i = replicate (sourceColumn (errorPos err) -1) ' ' ++ "^\n"
                               in Left $ str ++ i ++ show err
                   Right x  -> Right x

hVar :: Parsec String () Expr
hVar = do
  l <- letter
  s <- many (choice [letter, digit])
  return $ case l:s of
             "False" -> Lit (LBool False)
             "True"  -> Lit (LBool True)
             str     -> Ref (Var str)

hLitInt :: Parsec String () Expr
hLitInt = do
  n <- many digit
  return $ Lit (LInt (read n))

hParen :: Parsec String () Expr
hParen = do
  char '(' <* spaces
  e <- hExpr <* spaces
  char ')'
  return e

hTerm :: Parsec String () Expr
hTerm = choice [ try hParen, try hVar, try hLitInt ]

hOp :: Parsec String () Binop
hOp = do
  o <- choice [string "+", string "-", string "*", string "=="] <* spaces
  let op = case o of
             "+"  -> Add
             "-"  -> Sub
             "*"  -> Mul
             "==" -> Eql
  return op

hLine :: Parsec String () Expr
hLine = do
  e <- hExpr
  spaces <* eof
  return e

hExpr :: Parsec String () Expr
hExpr = do
  l <- hTerm <* spaces
  r <- many hExprN
  let f (op, right) left = Op op left right
  let x = foldr f l r
  return x

-- hExprN :: Parsec String () Expr
hExprN = do
  o <- hOp <* spaces
  r <- hExpr
  return (o, r)
