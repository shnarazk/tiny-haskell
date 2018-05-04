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
subst' e (t1, t2)
  | t1 == t2 = e
  | otherwise = Nothing

-- | 型変数を型で置き換える型代入
tySubst :: TypeEnv -> TSubst -> TypeEnv
tySubst e (tv, ty) = within (map shaper) <$> e
  where
    shaper (v, TScheme l t) = (v, TScheme (delete tv l) (lookdown t))
    lookdown :: Type -> Type
    lookdown t'@(TCon _) = t'
    lookdown t'@(TVar tv') = if tv' == tv then ty else t'
    lookdown (TLst t')    = TLst (lookdown t')
    lookdown (TTpl l')    = TTpl (map lookdown l')
    lookdown (TArr l')    = TArr (map lookdown l')

update :: TypeEnv -> [TSubst] -> TypeEnv
update e l = foldl tySubst e l

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
infer e (Lit (LInt _))    = return (TInt, e)
infer e (Lit (LBool _))   = return (TBool, e)
infer e (Lit (LString _)) = return (TString, e)
infer e (Ref v@(Var n)) = case find ((v ==) . fst) . unEnv =<< e of
                            Just (_, TScheme _ t) -> return (t, e)
                            Nothing -> do t <- newTypeVar  -- t :: TVar
                                          let s = TScheme [t] (TVar t)
                                          return (TVar t, extend e (Var n, s))
infer e (Pair ls) = do
  let loop [] ts e' = return (TTpl (reverse ts), e')
      loop (x:xs) ts e0 = do
        (t, e1) <- infer e0 x
        when (isNothing e1) $ throwError (UnificationFail x t t)
        loop xs (t:ts) e1
  loop ls [] e
infer e xp@(List ls) = do
  let loop [] t e' = return (TLst t, e')
      loop (x:xs) t e0 = do
        (t', e1) <- infer e0 x
        case unifier t t' of
          Just u -> do
            let e2 = update e1 u
            when (isNothing e2) $ throwError (UnificationFail xp t t')
            (t1, _) <- infer e2 x
            loop xs t1 e2
          Nothing -> throwError (UnificationFail xp t t')
  v <- newTypeVar
  loop ls (TVar v) e
infer e0 (App xs) = do
  let loop []    e ts = return (reverse ts, e)
      loop (a:l) e ts = do (t', e') <- infer e a
                           loop l e' (t' : ts)
  (ts, e1) <- loop xs e0 []
  r2 <- TArr . (\v -> tail ts ++ [v]) . TVar <$> newTypeVar
  let r1 = head ts
  case unifier r1 r2 of
    Just u  -> do let e2 = update e1 u
                  (ret, e3) <- infer e2 (head xs)
                  case ret of
                    TArr l -> return (last l, e3)
                    _      -> return (ret, e3)
    Nothing -> error ("aa" ++ show (head xs, r1, r2, unifier r1 r2)) -- throwError (UnificationFail (head xs) r1 r2)
infer e (Paren x) = infer e x
infer e0 xp@(Op op x y)
  | elem op [Add, Sub, Mul]  = do
      (tx, e1) <- infer e0 x
      case unifier tx TInt of
        Just u -> do let e2 = update e1 u
                     (ty, e3) <- infer e2 y
                     case unifier ty TInt of
                       Just w -> do let e4 = update e3 w
                                    when (isNothing e4) $ error ("aaaa" ++ show (e2, e3, y, ty, w))
                                    -- (Just E{x :: (Int -> Int)},Nothing,App [Ref x,Lit 60],t4,Int)
                                    return (TInt, e4)
                       Nothing -> throwError (UnificationFail y ty TInt)
        Nothing -> throwError (UnificationFail x tx TInt)
{-
      let e2 = subst e1 [(tx, TInt)]
      when (isNothing e2) $ error (show (e0, tx, unifier tx TInt, e2)) -- throwError (UnificationFail xp tx TInt)
      (ty, e3) <- infer e2 y
      let e4 = subst e3 [(ty, TInt)]
      when (isNothing e4) $ error (show (e2, ty, unifier ty TInt, e3)) -- throwError (UnificationFail xp ty TInt)
      return (TInt, e4)
-}
  | elem op [Eql] = do
      (tx, e1) <- infer e0 x
      (ty, e2) <- infer e1 y
      case unifier tx ty of
        Just u -> do
          let e3 = update e2 u
          when (isNothing e3) $ throwError (UnificationFail xp tx ty)
          return (TBool, e3)
        Nothing -> throwError (UnificationFail xp tx ty)
  | otherwise = throwError (UnificationFail xp TUnit TUnit)
infer e0 (Let v x1 x2) = do
  (t1, e1) <- infer e0 x1                    -- x1の型からvはt1型である（自由変数が消えるようなunifyは不可）
  (_ , e2) <- infer e1 x2                    -- 最初から自由変数がなければ消えたりはしない。
  let (Just (TScheme _ tv)) = schemeOf e2 v  -- x2での型推論よりvの型はtvでなければならない
  let (Just overlap) = not . null . intersect (tVarsIn t1) . freevars <$> e2   -- x1より束縛変数を求める
  case (not overlap, unifier t1 tv) of
    (True, Just u) -> do (tl, e3) <- infer (update e2 u) x2
                         return (tl, within (filter ((v /=) . fst)) <$> e3)
    _              -> throwError $ UnificationFail (Ref v) t1 tv

infer _ x = throwError $ NotImplemented x

tVarsIn :: Type -> [TVar]
tVarsIn (TCon _) = []
tVarsIn (TVar v) = [v]
tVarsIn (TLst t) = tVarsIn t
tVarsIn (TTpl l) = nub $ concatMap tVarsIn l
tVarsIn (TArr l) = nub $ concatMap tVarsIn l

-- TODO: add occurence checking
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
unifier (TTpl l1) (TTpl l2) = mconcat $ zipWith unifier l1 l2 -- TArr <$> zipWithM unifier l1 l2
unifier (TArr l1) (TArr l2) = mconcat $ zipWith unifier l1 l2 -- TArr <$> zipWithM unifier l1 l2
unifier _ _ = Nothing
{-
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
unifier (TVar _) t2@(TLst _) = Just t2
unifier (TVar _) t2@(TTpl _) = Just t2
unifier (TVar _) t2@(TArr _) = Just t2
unifier t1 t2@(TVar _) = unifier t2 t1
-- Compound types
unifier (TLst t1) (TLst t2) = TLst <$> unifier t1 t2
unifier (TTpl l1) (TTpl l2) = TArr <$> zipWithM unifier l1 l2
unifier (TArr l1) (TArr l2) = TArr <$> zipWithM unifier l1 l2
unifier _ _ = Nothing
-}
