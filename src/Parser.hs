module Parser ( runHaskell
              ) where

import Data.Functor
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Lib

runHaskell :: String -> Either String Expr
runHaskell str = case parse hLine "ERROR" str of
                   Left err -> let -- l = lines str !! (sourceLine (errorPos err) - 2)
                                   i = replicate (sourceColumn (errorPos err) -1) ' ' ++ "^\n"
                               in Left $ str ++ i ++ show err
                   Right x  -> Right x

lexer       = P.makeTokenParser haskellDef
parens      = P.parens lexer
-- braces      = P.braces lexer
brackets    = P.brackets lexer
identifier  = P.identifier lexer
integer     = P.integer lexer
symbol      = P.symbol lexer

hLine :: Parsec String () Expr
hLine = hExpr <* eof

hExpr  = hLet <|> hExpr'  `chainl1` eqlop
hExpr' = hTerm   `chainl1` addop
hTerm  = hFactor `chainl1` mulop

eqlop :: Parsec String () (Expr -> Expr -> Expr)
eqlop = symbol "==" $> Op Eql

mulop :: Parsec String () (Expr -> Expr -> Expr)
mulop = symbol "*" $> Op Mul

addop :: Parsec String () (Expr -> Expr -> Expr)
addop =   (symbol "+" $> Op Add)
      <|> (symbol "-" $> Op Sub)

hVar = f <$> identifier
  where f "False" = Lit (LBool False)
        f "True"  = Lit (LBool True)
        f str     = Ref (Var str)

hLitInt = Lit . LInt . fromInteger <$> integer

hList = brackets $ List <$> sepBy hExpr (symbol ",")

hParens = parens $ f <$> sepBy1 hExpr (symbol ",")
  where f l = if length l == 1 then Paren (head l) else Pair l

hFactor =  hList <|> hParens <|> hVar <|> hLitInt

hLet = do
  symbol "let"
  e0 <- hVar
  symbol "="
  e1 <- hExpr
  symbol "in"
  e2 <- hExpr
  case e0 of
    Ref v -> return $ Let v e1 e2
    _     -> fail "invalid var to assign"
