{-# LANGUAGE
    ViewPatterns
#-}
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
  prettyPrint e = pp 0 e
  precedenceOf :: s -> Int
  enclose :: Int -> s -> String
  enclose parent body@(precedenceOf -> self) = if parent > self then "(" ++ str ++ ")" else str
    where str = pp self body
  pp :: Int -> s -> String

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
  | Decl Var [Var] Expr
  | NullExpr
  deriving (Eq, Ord, Show)

instance PrettyPrint Expr where
  precedenceOf (Ref _) = maxBound
  precedenceOf (Lit _) = maxBound
  precedenceOf (App _) = 10
  precedenceOf (List _) = 0
  precedenceOf (Pair _) = 0
  precedenceOf (Paren _) = 0
  precedenceOf (Op op _ _) = precedenceOf op
  precedenceOf (Let v _ _) = 5
  precedenceOf (Decl v _ _) = 1
  precedenceOf NullExpr = maxBound
  pp n (Ref v) = pp 0 v
  pp n (Lit l) = pp 0 l
  pp n (App l) = intercalate " " (map (enclose n) l)
  pp n (List l) = "[" ++ intercalate ", " (map (pp 0) l) ++ "]"
  pp n (Pair l) = "(" ++ intercalate ", " (map (pp 0) l) ++ ")"
  pp n (Paren e) = "(" ++ pp 0 e ++ ")"
  pp n (Op x e1 e2) = pp n e1 ++ " " ++ pp n x ++ " " ++ pp n e2
  pp n (Let v e1 e2) = "let " ++ pp 0 v ++ " = " ++ pp 0 e1 ++ " in " ++ pp 0 e2
  pp n (Decl l as e) = intercalate " " (map (pp n) (l : as)) ++ " = " ++ pp n e
  pp n NullExpr = "_|_"

data Lit
  = LInt Int
  | LBool Bool
  | LString String
  | LFunc String
  deriving (Eq, Ord, Show)

instance PrettyPrint Lit where
  precedenceOf _ = maxBound
  pp _ (LInt n)    = show n
  pp _ (LBool b)   = show b
  pp _ (LString s) = s
  pp _ (LFunc f)   = f

data Var = Var Name
  deriving (Eq, Ord, Show)

instance PrettyPrint Var where
  precedenceOf _ = maxBound
  pp _ (Var name) = name

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)

instance PrettyPrint Binop where
  precedenceOf op
    | op == Eql = 2
    | op `elem` [Add, Sub] = 3
    | op == Mul = 4
  pp _ Add = "+"
  pp _ Sub = "-"
  pp _ Mul = "*"
  pp _ Eql = "=="

-- data Program = Program [Decl] Expr
--   deriving Eq

-- type Decl = (String, Expr)
