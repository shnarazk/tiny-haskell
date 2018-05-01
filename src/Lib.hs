{-# LANGUAGE
    DeriveFunctor
  , GeneralizedNewtypeDeriving
  , MultiWayIf
  , TupleSections
  #-}
-- http://dev.stephendiehl.com/fun/006_hindley_milner.html
-- https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter7/poly/src/Infer.hs

module Lib
    ( Expr(..)
    , runInfer
    , infer
     -- * Testing
    , Lit(..)
    , Var(..)
    , Binop(..)
    , TVar(..)
    , Type(..)
    , VarMap(..)
    , typeInt
    , typeBool
    , typeChar
    , TScheme(..)
    , Typing
    , TSubst
    , TypeEnv
    , emptyEnv
    , tySubst
    , schemeOf
    ) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State

import Data.List
import Data.Maybe
type Name = String

data Expr
  = Ref Var
  | FA Var [Expr]               -- Function Application
--  | App Expr Expr
  | Lit Lit
  | List [Expr]
  | Pair [Expr]
  | Op Binop Expr Expr
  deriving (Show, Eq, Ord)

data Lit
  = LInt Int
  | LBool Bool
  | LString String
  | LFunc String
  deriving (Eq, Ord, Show)

data Var = Var Name
  deriving (Eq, Ord)

instance Show Var where
  show (Var name) = name

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)

data Program = Program [Decl] Expr
  deriving Eq

type Decl = (String, Expr)

newtype TVar = TV Int
  deriving (Eq, Ord)

instance Show TVar where
  show (TV name) = "t" ++ show name

data Type
 = TCon Name
 | TVar TVar
 | TList Type
 | TPair [Type]
 | TArr [Type]
 deriving (Show, Eq, Ord)

typeInt :: Type
typeInt = TCon "Int"

typeBool :: Type
typeBool = TCon "Bool"

typeChar :: Type
typeChar = TCon "Char"

-- x         => TScheme [TV 1] (TV 1)
-- f x = x   => TScheme [TV 1] (TArr [(TV 1), (TV 1)])
-- f x y = y => TScheme [TV 1, TV 2] (TArr [(TV 1), (TV 1), (TV 1)])
data TScheme =
  TScheme { bounds  :: [TVar]
          , derived :: Type
          }
  deriving (Eq, Ord, Show)

type Typing = (Var, TScheme)
type TSubst = (TVar, Type)      -- 型代入

newtype VarMap = VarMap { unEnv :: [Typing] }
  deriving (Eq, Ord, Show)

within :: ([Typing] -> [Typing]) -> VarMap -> VarMap
within f (VarMap m) = VarMap $ f m

type TypeEnv = Maybe VarMap

schemeOf :: TypeEnv -> Var -> Maybe TScheme
schemeOf Nothing _ = Nothing
schemeOf (Just e) v = snd <$> find ((v ==) . fst) (unEnv e)

without :: TypeEnv -> Var -> TypeEnv
without e v = VarMap . del . unEnv <$> e
  where
    del :: [(Var, TScheme)] -> [(Var, TScheme)]
    del [] = []
    del (x@(var, _):ns)
      | var == v  = del ns
      | otherwise = x : del ns

extend :: TypeEnv -> Typing -> TypeEnv
extend e (x, s) = within ((x, s) :) <$> e

-- 環境は1つ。そこに束縛対を追加しようとする。
-- 返値は新規に追加すべき束縛対、および型エラーなく融合するための置換ルール。
injectScheme :: TypeEnv -> Typing -> Maybe ([Typing], [TSubst])
injectScheme e (v, s)
  | Just s' <- schemeOf e v = ([],) <$> unifyingSchemes s s'  -- 融合のための置換ルール
  | otherwise               = Just ([(v, s)], [])             -- 存在しなければ追加

unifyingSchemes :: TScheme -> TScheme -> Maybe [TSubst]
-- 1. identical patterns
unifyingSchemes (TScheme l1 t1@(TVar u1)) (TScheme l2 t2@(TVar u2))
  | notElem u1 l1 = Just [(u2, t1)]  -- u1 is global
  | otherwise     = Just [(u1, t2)]  -- u2 is global, or tie break
unifyingSchemes (TScheme _ (TCon tc)) (TScheme _ (TCon tc'))
  | tc == tc' = Just []
  | otherwise  = Nothing
-- 2. map to a single type variable
unifyingSchemes (TScheme _ (TVar tv)) (TScheme _ t@(TCon _)) = Just [(tv, t)]
-- 2. map to a single type variable (reflected)
unifyingSchemes s1 s2@(TScheme _ (TVar _)) = unifyingSchemes s2 s1

-- | 型を型で置き換える型代入
subst :: TypeEnv -> (Type, Type) -> TypeEnv
subst e (TCon t1, TCon t2)
  | t1 == t2 = e
  | otherwise = Nothing
subst e (TVar tv@(TV _), t2) = tySubst e (tv, t2)
subst e x = Nothing

-- | 型変数を型で置き換える型代入
-- >>> t = VarMap [(Var "x", TScheme [] (TCon "Int")), (Var "y", TScheme [TV 1] (TVar (TV 1)))]
-- >>> tySubst t (TV 1, TCon "Int")
-- VarMap {unEnv = [(Var "x",TScheme [] (TCon "Int")),(Var "y",TScheme [] (TCon "Int"))]}
-- >>> tySubst t (TV 2, TCon "Int")
-- VarMap {unEnv = [(Var "x",TScheme [] (TCon "Int")),(Var "y",TScheme [TV 1] (TVar (TV 1)))]}
-- >>> tySubst t (TV 1, TV 2)
-- VarMap {unEnv = [(Var "x",TScheme [] (TCon "Int")),(Var "y",TScheme [TV 1] (TVar (TV 1)))]}
tySubst :: TypeEnv -> TSubst -> TypeEnv
tySubst e (tv, ty) = within (map shaper) <$> e
  where
    shaper (v, TScheme l t) = (v, TScheme (delete tv l) (lookdown t))
      where lookdown :: Type -> Type
            lookdown t'@(TCon _) = t'
            lookdown t'@(TVar tv') = if tv' == tv then ty else t'
            lookdown (TList t')    = TList (lookdown t')
            lookdown (TPair l')    = TPair (map lookdown l')
            lookdown (TArr l')     = TArr (map lookdown l')

update :: TypeEnv -> ([Typing], [TSubst]) -> TypeEnv
update e (tl, sl) = foldl tySubst (foldl extend e tl) sl

-- extend emptyEnv (Var "x", TScheme [] typeInt)
emptyEnv :: TypeEnv
emptyEnv = Just $ VarMap []

-- Subst in a restricted 'TypeEnv' without 'TScheme'
-- type Subst = [(TVar, Type)]

data TypeError
  = UnificationFail Expr Type Type
  | InfiniteType TVar Expr Type
  | UnboundVariable Expr String
  deriving (Eq, Show)

type InferResult = Either TypeError (Type, TypeEnv)

type Infer a = StateT Int (ExceptT TypeError Identity) a

runInfer :: Expr -> InferResult
runInfer e = runIdentity $ runExceptT (evalStateT (infer emptyEnv e) 0)

newTypeVar :: Infer TVar
newTypeVar = do i <- get
                let j = i + 1
                put j
                return $ TV j

-- | 型環境下における与えられた式の型を返す
-- | 型環境は部分式の評価において更新されるため、更新された型環境も返す必要がある。
infer :: TypeEnv -> Expr -> Infer (Type, TypeEnv)
infer e (Lit (LInt _)) = return (TCon "Int", e)
infer e (Lit (LBool _)) = return (TCon "Bool", e)
infer e (Lit (LString _)) = return (TList (TCon "Char"), e)
infer e (Ref (Var n)) = do t <- newTypeVar  -- t :: TVar
                           let s = TScheme [t] (TVar t)
                           return $ (TVar t, extend e (Var n, s))
infer e0 exp@(Op op x y)
  | elem op [Add, Sub, Mul]  = do (tx, e1) <- infer e0 x
                                  let e2 = subst e1 (tx, typeInt)
                                  when (isNothing e2) $ throwError (UnificationFail x tx typeInt)
                                  (ty, e2) <- infer e2 y
                                  let e3 = subst e2 (ty, typeInt)
                                  when (isNothing e2) $ throwError (UnificationFail y ty typeInt)
                                  return $ (typeInt, e3)
  | elem op [Eql]            = do (tx, e1) <- infer e0 x
                                  (ty, e2) <- infer e1 y
                                  let e3 = subst e2 (tx, ty)
                                  when (isNothing e3) $ throwError (UnificationFail exp tx ty)
                                  return $ (tx, e3)
infer e x = error $ show (e, x)
