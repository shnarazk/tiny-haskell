module AST
  (
    Expr(..)
  , Name
  , Lit(..)
  , Var(..)
  , Binop(..)
  -- * For debug
  , PrettyPrint(..)
  ) where

import Data.Functor
import Data.List
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

class PrettyPrint s where
  prettyPrint :: s -> String

-------------------------------------------------------------------------------- Expression
type Name = String

data Expr
  = Ref Var
--  | App Expr Expr
  | Lit Lit
  | App [Expr]               -- Function Application
  | List [Expr]
  | Pair [Expr]
  | Paren Expr
  | Op Binop Expr Expr
  | Let Var Expr Expr
  | NullExpr
  deriving (Eq, Ord, Show)

instance PrettyPrint Expr where
  prettyPrint (Ref v) = prettyPrint v
  prettyPrint (Lit l) = prettyPrint l
  prettyPrint (App l) = intercalate " " (map prettyPrint l)
  prettyPrint (List l) = "[" ++ intercalate ", " (map prettyPrint l) ++ "]"
  prettyPrint (Pair l) = "(" ++ intercalate ", " (map prettyPrint l) ++ ")"
  prettyPrint (Paren e) = "(" ++ prettyPrint e ++ ")"
  prettyPrint (Op x e1 e2) = " (" ++ prettyPrint e1 ++ " " ++ prettyPrint x ++ " " ++ prettyPrint e2 ++ ") "
  prettyPrint (Let v e1 e2) = "let " ++ prettyPrint v ++ " = " ++ prettyPrint e1 ++ " in " ++ prettyPrint e2
  prettyPrint NullExpr = "_|_"

data Lit
  = LInt Int
  | LBool Bool
  | LString String
  | LFunc String
  deriving (Eq, Ord, Show)

instance PrettyPrint Lit where
  prettyPrint (LInt n) = show n
  prettyPrint (LBool b) = show b
  prettyPrint (LString s) = s
  prettyPrint (LFunc f) = f

data Var = Var Name
  deriving (Eq, Ord, Show)

instance PrettyPrint Var where
  prettyPrint (Var name) = name

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)

instance PrettyPrint Binop where
  prettyPrint Add = "+"
  prettyPrint Sub = "-"
  prettyPrint Mul = "*"
  prettyPrint Eql = "=="

data Program = Program [Decl] Expr
  deriving Eq

type Decl = (String, Expr)
