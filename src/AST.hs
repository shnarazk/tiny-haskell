module AST
  ( Expr(..)
  , Name
  , Lit(..)
  , Var(..)
  , Binop(..)
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
  | NullExpr
  deriving (Eq, Ord)

instance Show Expr where
  show (Ref v) = show v
  show (Lit l) = show l
  show (List l) = "[" ++ intercalate ", " (map show l) ++ "]"
  show (Pair l) = "(" ++ intercalate ", " (map show l) ++ ")"
  show (Paren e) = "(" ++ show e ++ ")"
  show (Op x e1 e2) = show e1 ++ " " ++ show x ++ " " ++ show e2
  show (Let v e1 e2) = "let " ++ show v ++ " = " ++ show e1 ++ " in " ++ show e2
  show NullExpr = "_|_"

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

