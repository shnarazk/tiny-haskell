{-# LANGUAGE
    DeriveFunctor
  , GeneralizedNewtypeDeriving
  , MultiWayIf
  , TupleSections
  #-}
-- http://dev.stephendiehl.com/fun/006_hindley_milner.html
-- https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter7/poly/src/Infer.hs

module TypeInfer
  ( TypeError(..)
  , runInfer
  , infer
    -- * Testing
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
import System.IO.Unsafe (unsafePerformIO)
import Syntax

-------------------------------------------------------------------------------- Type system

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
 deriving (Eq, Ord)

instance Show Type where
  show (TCon n) = n
  show (TVar t) = show t
  show (TList l) = show [l]
  show (TPair l) = "(" ++ intercalate ", " (map show l) ++ ")"
  show (TArr l) = "(" ++ intercalate " -> " (map show l) ++ ")"

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
  deriving (Eq, Ord)

instance Show TScheme where
  show (TScheme [] t) = "S:" ++ show t
  show (TScheme tvs t) = "S" ++ show (sort tvs) ++ "." ++ show t

type Typing = (Var, TScheme)
type TSubst = (TVar, Type)      -- 型代入

newtype VarMap = VarMap { unEnv :: [Typing] }
  deriving (Eq, Ord)

instance Show VarMap where
  show (VarMap l) = "E{" ++ intercalate ", " (map f (sort l)) ++ "}"
    where f (v, TScheme [] t) = show v ++ " :: " ++ show t
          f (v, s) = show v ++ " :: " ++ show s

within :: ([Typing] -> [Typing]) -> VarMap -> VarMap
within f (VarMap m) = VarMap $ f m

type TypeEnv = Maybe VarMap

schemeOf :: TypeEnv -> Var -> Maybe TScheme
schemeOf Nothing _ = Nothing
schemeOf (Just e) v = snd <$> find ((v ==) . fst) (unEnv e)

extend :: TypeEnv -> Typing -> TypeEnv
extend e (x, s) = within ((x, s) :) <$> e

class HasFreeVars s where
  freevars :: s -> [TVar]

instance HasFreeVars Type where
  freevars (TCon _)  = []
  freevars (TVar x)  = [x]
  freevars (TList t) = freevars t
  freevars (TPair l) = nub $ concatMap freevars l
  freevars (TArr l)  =  nub $ concatMap freevars l

instance HasFreeVars TScheme where
  freevars (TScheme tl t) = freevars t \\ tl

instance HasFreeVars VarMap where
  freevars e = nub $ concatMap (freevars .snd) (unEnv e)

overlapped :: VarMap -> TScheme -> Bool
overlapped e (TScheme vs _) = null $ intersect (freevars e)  vs

-- | 型を型で置き換える型代入
subst :: TypeEnv -> [(Type, Type)] -> TypeEnv
subst e l = foldl subst' e l

subst' :: TypeEnv -> (Type, Type) -> TypeEnv
subst' Nothing _ = Nothing
subst' e (TCon t1, TCon t2)
  | t1 == t2 = e
  | otherwise = Nothing
subst' e (TVar tv@(TV _), t2) = tySubst e (tv, t2)
subst' e (t1, TVar tv@(TV _)) = tySubst e (tv, t1)
subst' e x = Nothing

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

data TypeError
  = UnificationFail Expr Type Type
  | InfiniteType TVar Expr Type
  | UnboundVariable Expr String
  | NotImplemented Expr
  deriving (Eq, Show)

type InferResult = Either TypeError (Type, TypeEnv)

type Infer a = StateT Int (ExceptT TypeError Identity) a

runInfer :: Expr -> InferResult
runInfer e = runIdentity $ runExceptT (evalStateT (infer emptyEnv e) 0)

newTypeVar :: Infer TVar
newTypeVar = do i <- (1 +) <$> get
                put i
                return $ TV i

-- | 型環境e下における与えられた式の型を返す
-- | 型環境は部分式の評価において更新されるため、更新された型環境も返す必要がある。
infer :: TypeEnv -> Expr -> Infer (Type, TypeEnv)
infer e (Lit (LInt _))    = return (TCon "Int", e)
infer e (Lit (LBool _))   = return (TCon "Bool", e)
infer e (Lit (LString _)) = return (TList (TCon "Char"), e)
infer e (Ref v@(Var n)) = case find ((v ==) . fst) . unEnv =<< e of
                            Just (_, TScheme _ t) -> return (t, e)
                            Nothing -> do t <- newTypeVar  -- t :: TVar
                                          let s = TScheme [t] (TVar t)
                                          return (TVar t, extend e (Var n, s))
infer e (Pair ls) = do
  let loop [] ts e' = return (TPair (reverse ts), e')
      loop (x:xs) ts e0 = do
        (t, e1) <- infer e0 x
        when (isNothing e1) $ throwError (UnificationFail x t t)
        loop xs (t:ts) e1
  loop ls [] e
infer e xp@(List ls) = do
  let loop [] t e' = return (TList t, e')
      loop (x:xs) t e0 = do
        (t', e1) <- infer e0 x
        case unifier t t' of
          Just u -> do
            let e2 = subst e1 [(t, u), (t', u)]
            when (isNothing e2) $ throwError (UnificationFail xp t t')
            loop xs u e2
          Nothing -> throwError (UnificationFail xp t t')
  v <- newTypeVar
  loop ls (TVar v) e
infer e (Paren x) = infer e x
infer e0 xp@(Op op x y)
  | elem op [Add, Sub, Mul]  = do
      (tx, e1) <- infer e0 x
      let e2 = subst e1 [(tx, typeInt)]
      when (isNothing e2) $ throwError (UnificationFail x tx typeInt)
      (ty, e3) <- infer e2 y
      when (isNothing e3) $ throwError (UnificationFail y ty typeInt)
      let e4 = subst e3 [(ty, typeInt)]
      return (typeInt, e4)
  | elem op [Eql] = do
      (tx, e1) <- infer e0 x
      (ty, e2) <- infer e1 y
      case unifier tx ty of
        Just u -> do
          let e3 = subst e2 [(tx, u), (ty, u)]
          when (isNothing e3) $ throwError (UnificationFail xp tx ty)
          return (typeBool, e3)
        Nothing -> throwError (UnificationFail xp tx ty)

infer _ x = throwError $ NotImplemented x

-- TODO: add occurence checking
unifier :: Type -> Type -> Maybe Type
-- Constant
unifier t1@(TCon _) t2@(TCon _)
  | t1 == t2 = Just t1
  | otherwise = Nothing
unifier t@(TCon _) (TVar _) = Just t
unifier (TCon _) _ = Nothing
unifier t1 t2@(TCon _) = unifier t2 t1
-- Var
unifier t1@(TVar _) (TVar _) = Just t1
unifier (TVar _) t2@(TList _) = Just t2
unifier (TVar _) t2@(TPair _) = Just t2
unifier (TVar _) t2@(TArr _) = Just t2
unifier t1 t2@(TVar _) = unifier t2 t1
-- Compound types
unifier (TList t1) (TList t2) = TList <$> unifier t1 t2
unifier (TPair l1) (TPair l2) = TArr <$> zipWithM unifier l1 l2
unifier (TArr l1) (TArr l2) = TArr <$> zipWithM unifier l1 l2
unifier _ _ = Nothing
