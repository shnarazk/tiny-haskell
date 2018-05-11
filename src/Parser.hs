module Parser
  ( parseHaskell
  ) where

import Control.Monad
import Data.Functor
import Data.List
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import AST

parseHaskell :: String -> Either String Expr
parseHaskell str =
  case parse hLine "ERROR" str of
    Left err -> let l = lines str !! (max (length (lines str)) (sourceLine (errorPos err) - 1) - 1)
                    i = replicate (sourceColumn (errorPos err) -1) ' ' ++ "^\n"
                in Left $ l ++ "\n" ++ i ++ show err
    Right x  -> Right x

lexer       = P.makeTokenParser haskellDef
parens      = P.parens lexer
brackets    = P.brackets lexer
identifier  = P.identifier lexer
symbol      = P.symbol lexer
operator    = P.operator lexer
integer     = P.natural lexer

hLine :: Parsec String () Expr
hLine = hDorE <* eof

hDorE = hExpr `chainr1` defop
hExpr = hExp' `chainr1` eqlop
hExp' = hTerm `chainr1` try addop
hTerm = hFact `chainr1` try mulop
hFact = hList <|> hParens <|> hLet <|> hLitInt <|> hAppl

hAppl  = do
  f <- hIdent
  args <- many (hList <|> parens hExpr <|> hLitInt <|> hIdent)
  return $ if null args then f else App (f : args)

defop = symbol "=" $> f
  where f (App l) x = let l' = map (\(Ref v) ->v) l in Decl (head l') (tail l') x
        f (Ref v) x = Decl v [] x

hFactor = hList <|> hParens <|> hLet <|> hLitInt <|> hIdent

eqlop :: Parsec String () (Expr -> Expr -> Expr)
eqlop = try (symbol "==") $> Op Eql

mulop :: Parsec String () (Expr -> Expr -> Expr)
mulop = do { symbol "*" ; return $ Op Mul }

addop :: Parsec String () (Expr -> Expr -> Expr)
addop =     do { symbol "+" ; return $ Op Add }
        <|> do { symbol "-" ; return $ Op Sub }

brank =  notFollowedBy operator $> f
  where f (App l) r = App $ l ++ [r]
        f l r       = App $ [l, r]

hList = brackets $ List <$> sepBy hExpr (symbol ",")

hParens = parens $ f <$> sepBy1 hExpr (symbol ",")
  where f l = if length l == 1 then Paren (head l) else Pair l

hLet = do
  symbol "let"
  e0 <- hIdent <* symbol "="
  e1 <- hExpr <* symbol "in"
  e2 <- hExpr
  case e0 of
    Ref v -> return $ Let v e1 e2
    _     -> fail "invalid var to assign"

hIdent = f <$> identifier
  where f "False" = Lit (LBool False)
        f "True"  = Lit (LBool True)
        f str     = Ref (Var str)

hLitInt = Lit . LInt . fromIntegral <$> integer
