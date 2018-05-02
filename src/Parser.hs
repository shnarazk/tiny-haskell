module Parser
  ( Expr(..)
  , Name
  , Lit(..)
  , Var(..)
  , Binop(..)
  , runHaskell
  ) where

import Data.Functor
import Data.List
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

-------------------------------------------------------------------------------- Expression

type Name = String

data Expr
  = Ref Var
  | FA Var [Expr]               -- Function Application
--  | App Expr Expr
  | Lit Lit
  | List [Expr]
  | Pair [Expr]
  | Paren Expr
  | Op Binop Expr Expr
  | Let Var Expr Expr
  deriving (Eq, Ord)

instance Show Expr where
  show (Ref v) = show v
  show (Lit l) = show l
  show (List l) = "[" ++ intercalate ", " (map show l) ++ "]"
  show (Pair l) = "(" ++ intercalate ", " (map show l) ++ ")"
  show (Paren e) = "(" ++ show e ++ ")"
  show (Op x e1 e2) = show e1 ++ " " ++ show x ++ " " ++ show e2
  show (Let v e1 e2) = "let " ++ show v ++ " = " ++ show e1 ++ " in " ++ show e2

data Lit
  = LInt Int
  | LBool Bool
  | LString String
  | LFunc String
  deriving (Eq, Ord)

instance Show Lit where
  show (LInt n) = show n
  show (LBool b) = show b
  show (LString s) = s
  show (LFunc f) = f

data Var = Var Name
  deriving (Eq, Ord)

instance Show Var where
  show (Var name) = name

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord)

instance Show Binop where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Eql = "=="

data Program = Program [Decl] Expr
  deriving Eq

type Decl = (String, Expr)

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
