module Parser
  ( parseHaskell
  ) where

import Data.Functor
import Data.List
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import AST

parseHaskell :: String -> Either String Expr
parseHaskell str = case parse hLine "ERROR" str of
                     Left err -> let l = lines str !! ((max (length (lines str)) (sourceLine (errorPos err) - 1)) -1)
                                     i = replicate (sourceColumn (errorPos err) -1) ' ' ++ "^\n"
                                 in Left $ l ++ "\n" ++ i ++ show err
                     Right x  -> Right x

lexer       = P.makeTokenParser haskellDef
parens      = P.parens lexer
-- braces      = P.braces lexer
brackets    = P.brackets lexer
identifier  = P.identifier lexer
integer     = P.integer lexer
symbol      = P.symbol lexer
operator    = P.operator lexer

hLine :: Parsec String () Expr
hLine = hExpr <* eof

hExpr  = hExpr'  `chainl1` eqlop
hExpr' = hTerm   `chainl1` addop
hTerm  = hAppl   `chainl1` mulop
hAppl  = hFactor `chainl1` brank
_hAppl  = do
  l <- many1 (hLet <|> hVar <|> hLitInt <|> hParens <|> hList)
  -- lookAhead (symbol "+")
  -- lookAhead (operator <|> (eof $> "") <|> symbol "+")
  case l of
    [e] -> return e
    l   -> return $ App l

--  where f [e] = e
--        f l   = App l

eqlop :: Parsec String () (Expr -> Expr -> Expr)
eqlop = symbol "==" $> Op Eql

mulop :: Parsec String () (Expr -> Expr -> Expr)
mulop = symbol "*" $> Op Mul

addop :: Parsec String () (Expr -> Expr -> Expr)
addop =   (symbol "+" $> Op Add)
      <|> (symbol "-" $> Op Sub)

brank = notFollowedBy operator $> f
  where f (App l) r = App $ l ++ [r]
        f l r       = App $ [l, r]

-- hFactor = hVar <|> hLitInt
hFactor = hList <|> hParens <|> hLet <|> hVar <|> hLitInt

hList = brackets $ List <$> sepBy hExpr (symbol ",")

hParens = parens $ f <$> sepBy1 hExpr (symbol ",")
  where f l = if length l == 1 then Paren (head l) else Pair l

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

hVar = f <$> identifier
  where f "False" = Lit (LBool False)
        f "True"  = Lit (LBool True)
        f str     = Ref (Var str)

hLitInt = Lit . LInt . fromInteger <$> integer

hApplication = do
  f <- hExpr
  App . (f :) <$> many1 hExpr
