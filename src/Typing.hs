{-# LANGUAGE
    DeriveFunctor
  , GeneralizedNewtypeDeriving
  , MultiWayIf
  , PatternSynonyms
  , TupleSections
  #-}
-- http://dev.stephendiehl.com/fun/006_hindley_milner.html
-- https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter7/poly/src/Infer.hs

module Typing
  ( TypeError(..)
  , runInfer
  , infer
    -- * Testing
  , TVar(..)
  , Type(.., TBool, TInt, TChar, TString, TUnit)
  , VarMap(..)
  , TypeEnv
  , emptyEnv
  , subst
  , TScheme(..)
  , schemeOf
  ) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State

import Data.List
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)
import AST

-------------------------------------------------------------------------------- Type system

newtype TVar = TV Int
  deriving (Eq, Ord)

instance Show TVar where
  show (TV name) = "t" ++ show name

data Type
 = TCon Name                    -- Constant
 | TVar TVar                    -- Variable
 | TLst Type                    -- List
 | TTpl [Type]                  -- Tuple
 | TArr [Type]                  -- Arror
 deriving (Eq, Ord)

instance Show Type where
  show (TCon n) = n
  show (TVar t) = show t
  show (TLst l) = show [l]
  show (TTpl l) = "(" ++ intercalate ", " (map show l) ++ ")"
  show (TArr l) = "(" ++ intercalate " -> " (map show l) ++ ")"

pattern TInt :: Type
pattern TInt = TCon "Int"
pattern TBool :: Type
pattern TBool = TCon "Bool"
pattern TChar :: Type
pattern TChar = TCon "Char"
pattern TString :: Type
pattern TString = TLst TChar
pattern TUnit :: Type
pattern TUnit = TCon "()"

-- x         => (TV x) : TScheme [TV 1] (TV 1)
-- f x = x   => (TV f) : TScheme [TV 1] (TArr [(TV 1), (TV 1)])
-- f x y = y => (TV f) : TScheme [TV 1, TV 2] (TArr [(TV 1), (TV 1), (TV 1)])
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
  freevars (TLst t) = freevars t
  freevars (TTpl l) = nub $ concatMap freevars l
  freevars (TArr l)  =  nub $ concatMap freevars l

instance HasFreeVars TScheme where
  freevars (TScheme tl t) = freevars t \\ tl

instance HasFreeVars VarMap where
  freevars e = nub $ concatMap (freevars .snd) (unEnv e)

-- | 型変数を型で置き換える型代入
subst :: TypeEnv -> [TSubst] -> TypeEnv
subst e l = foldl subst1 e l

subst1 :: TypeEnv -> TSubst -> TypeEnv
subst1 e (tv, ty) = within (map shaper) <$> e
  where
    shaper (v, TScheme l t) = (v, TScheme (delete tv l) (lookdown t))
    lookdown :: Type -> Type
    lookdown t'@(TCon _) = t'
    lookdown t'@(TVar tv') = if tv' == tv then ty else t'
    lookdown (TLst t')    = TLst (lookdown t')
    lookdown (TTpl l')    = TTpl (map lookdown l')
    lookdown (TArr l')    = TArr (map lookdown l')

emptyEnv :: TypeEnv
emptyEnv = Just $ VarMap []

data TypeError
  = UnificationFail Expr Type Type
  | InfiniteType Expr TVar Type
  | UnboundVariable Expr String
  | NotImplemented Expr
  deriving (Eq, Show)

type InferResult = Either TypeError (Type, TypeEnv)

type Infer a = StateT Int (ExceptT TypeError Identity) a

runInfer :: Expr -> InferResult
runInfer e = runIdentity $ runExceptT (evalStateT (infer e emptyEnv) 0)

newTypeVar :: Infer TVar
newTypeVar = do { i <- (1 +) <$> get; put i; return $ TV i }

-- | 型環境e下における与えられた式の型を返す
-- | 型環境は部分式の評価において更新されるため、更新された型環境も返す必要がある。
infer :: Expr -> TypeEnv -> Infer (Type, TypeEnv)
infer (Lit (LInt _))    e = return (TInt, e)
infer (Lit (LBool _))   e = return (TBool, e)
infer (Lit (LString _)) e = return (TString, e)
infer (Ref v@(Var n))   e = case find ((v ==) . fst) . unEnv =<< e of
                              Just (_, TScheme _ t) -> return (t, e)
                              Nothing -> do t <- newTypeVar  -- t :: TVar
                                            let s = TScheme [t] (TVar t)
                                            return (TVar t, extend e (Var n, s))
infer (Pair ls) e = do
  let loop [] ts e' = return (TTpl (reverse ts), e')
      loop (x:xs) ts e0 = do { (t, e1) <- infer x e0; loop xs (t:ts) e1 }
  loop ls [] e
infer xp@(List ls) e = do
  let loop [] e' t = return (TLst t, e')
      loop (x:xs) e0 t = do { (t1, e2) <- infer x =<< uncurry (unify xp t) =<< infer x e0; loop xs e2 t1 }
  loop ls e . TVar =<< newTypeVar
infer (App xs) e0 = do
  let loop []    e ts = return (reverse ts, e)
      loop (a:l) e ts = do { (t', e') <- infer a e; loop l e' (t' : ts) }
  (ts, e1) <- loop xs e0 []
  r2 <- TArr . (\v -> tail ts ++ [v]) . TVar <$> newTypeVar
  (TArr l, e2) <- infer (head xs) =<< unify (head xs) (head ts) r2 e1
  case drop (length xs - 1) l of
    []  -> throwError $ UnificationFail (App xs) TUnit (TArr l)
    [e] -> return (e, e2)
    l   -> return  (TArr l, e2)
infer (Paren x) e = infer x e
infer xp@(Op op x y) e0
  | elem op [Add, Sub, Mul]  = do (tx, e1) <- infer x e0
                                  (ty, e2) <- infer y =<< unify x tx TInt e1
                                  (TInt,) <$> unify y ty TInt e2
  | elem op [Eql] = do (tx, e1) <- infer x e0
                       (ty, e2) <- infer y e1
                       (TBool,) <$> unify xp tx ty e2
  | otherwise = throwError $ NotImplemented xp
infer (Let v x1 x2) e0 = do
  (t1, e1) <- infer x1 e0                    -- x1の型からvはt1型である（自由変数が消えるようなunifyは不可）
  (_ , e2) <- infer x2 e1                    -- 最初から自由変数がなければ消えたりはしない。
  let (Just (TScheme _ tv)) = schemeOf e2 v  -- x2での型推論よりvの型はtvでなければならない
  let (Just overlap) = not . null . intersect (tVarsIn t1) . freevars <$> e2   -- x1より束縛変数を求める
  if overlap
    then throwError $ UnificationFail (Ref v) t1 tv
    else do e3 <- unify (Ref v) t1 tv e2
            (tl, e4) <- infer x2 e3
            return (tl, within (filter ((v /=) . fst)) <$> e4)
infer x _ = throwError $ NotImplemented x

tVarsIn :: Type -> [TVar]
tVarsIn (TCon _) = []
tVarsIn (TVar v) = [v]
tVarsIn (TLst t) = tVarsIn t
tVarsIn (TTpl l) = nub $ concatMap tVarsIn l
tVarsIn (TArr l) = nub $ concatMap tVarsIn l

unifier :: Type -> Type -> Maybe [TSubst]
-- Constant
unifier t1@(TCon _) t2@(TCon _)
  | t1 == t2 = Just []
  | otherwise = Nothing
unifier t@(TCon _) (TVar v) = Just [(v, t)]
unifier (TCon _) _ = Nothing
unifier t1 t2@(TCon _) = unifier t2 t1
-- Var
unifier t1@(TVar _) (TVar t2) = Just [(t2, t1)]
unifier (TVar t1) t2@(TLst _) = Just [(t1, t2)]
unifier (TVar t1) t2@(TTpl _) = Just [(t1, t2)]
unifier (TVar t1) t2@(TArr _) = Just [(t1, t2)]
unifier t1 t2@(TVar _) = unifier t2 t1
-- Compound types
unifier (TLst t1) (TLst t2) = unifier t1 t2
unifier (TTpl l1) (TTpl l2)
  | length l1 == length l2 = mconcat $ zipWith unifier l1 l2
  | otherwise = Nothing
unifier (TArr l1) (TArr l2)
  | length l1 == length l2 = mconcat $ zipWith unifier l1 l2
  | otherwise = Nothing
unifier _ _ = Nothing

unify :: Expr -> Type -> Type -> TypeEnv -> Infer TypeEnv
unify x t t' e
  | TVar v <- t, occurrenceCheck v t' = throwError (InfiniteType x v t')
  | otherwise =
    case unifier t t' of
      Just u -> return $ subst e u
      Nothing -> throwError (UnificationFail x t t')

occurrenceCheck :: TVar -> Type -> Bool
occurrenceCheck t1 t2@(TLst _) = elem t1 (freevars t2)
occurrenceCheck t1 t2@(TTpl _) = elem t1 (freevars t2)
occurrenceCheck t1 t2@(TArr _) = elem t1 (freevars t2)
occurrenceCheck _ _ = False
