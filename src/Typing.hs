{-# LANGUAGE
    FlexibleInstances
  , PatternSynonyms
  , TupleSections
  , TypeSynonymInstances
  #-}
module Typing
  ( -- * Type System
    Type(.., TBool, TInt, TChar, TString, TUnit)
  , TVar(..)
    -- * Type Inference
  , TypeError(..)
  , inferExpr
    -- * Type Environment
  , TypeEnv
  , emptyEnv
  , haskellEnv
  , TScheme(..)
    -- * Debugging stuff
  , subst
  , shadow
  , schemeOf
  ) where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State
import Data.List
import AST

-------------------------------------------------------------------------------- Type system
class HasFreeVars s where
  freevars :: s -> [TVar]

newtype TVar = TV Int
  deriving (Eq, Ord, Show)

instance PrettyPrint TVar where
  prettyPrint (TV name) = "t" ++ show name

data Type
 = TCon Name                    -- Constant
 | TVar TVar                    -- Variable
 | TArr [Type]                  -- Arror
 deriving (Eq, Ord, Show)

instance PrettyPrint Type where
  prettyPrint (TCon n) = n
  prettyPrint (TVar t) = prettyPrint t
  prettyPrint (TArr l) = "(" ++ intercalate " -> " (map prettyPrint l) ++ ")"

instance HasFreeVars Type where
  freevars (TCon _)  = []
  freevars (TVar x)  = [x]
  freevars (TArr l)  =  nub $ concatMap freevars l

pattern TInt    :: Type
pattern TInt    = TCon "Int"
pattern TBool   :: Type
pattern TBool   = TCon "Bool"
pattern TChar   :: Type
pattern TChar   = TCon "Char"
pattern TString :: Type
pattern TString = TCon "String"
pattern TUnit   :: Type
pattern TUnit   = TCon "()"

tVarsIn :: Type -> [TVar]
tVarsIn (TCon _) = []
tVarsIn (TVar v) = [v]
tVarsIn (TArr l) = nub $ concatMap tVarsIn l

-- x         => (TV x) : TScheme [TV 1] (TV 1)
-- f x = x   => (TV f) : TScheme [TV 1] (TArr [(TV 1), (TV 1)])
-- f x y = y => (TV f) : TScheme [TV 1, TV 2] (TArr [(TV 1), (TV 1), (TV 1)])
data TScheme =
  TScheme { bounds  :: [TVar]
          , derived :: Type
          }
  deriving (Eq, Ord, Show)

instance HasFreeVars TScheme where
  freevars (TScheme tl t) = freevars t \\ tl

instance PrettyPrint TScheme where
  prettyPrint (TScheme [] t) = "S:" ++ prettyPrint t
  prettyPrint (TScheme vs t) = "S" ++ intercalate "." (map prettyPrint (sort vs)) ++ "." ++ prettyPrint t

-------------------------------------------------------------------------------- TypeEnv
type Typing = (Var, TScheme)

newtype Tagged a = Tagged { unWrap :: a }
  deriving (Eq, Ord, Show)

instance Functor Tagged where
  fmap f = Tagged . f . unWrap

type TypeEnv = Tagged [Typing]

instance PrettyPrint TypeEnv where
  prettyPrint e
    | null env  = "EmptyEnviroment"
    | otherwise = "E{" ++ intercalate ", " (map f (sort env)) ++ "}"
    where f (v, TScheme [] t) = prettyPrint v ++ " :: " ++ prettyPrint t
          f (v, s) = prettyPrint v ++ " :: " ++ prettyPrint s
          env = unEnv e

makeEnv :: [Typing] -> TypeEnv
makeEnv = Tagged

emptyEnv :: TypeEnv
emptyEnv = makeEnv []

unEnv :: TypeEnv -> [Typing]
unEnv = unWrap

schemeOf :: TypeEnv -> Var -> Maybe TScheme
schemeOf e v = snd <$> find ((v ==) . fst) (unEnv e)

extend :: TypeEnv -> Typing -> TypeEnv
extend e (x, s) = fmap ((x, s) :) e

shadow :: TypeEnv -> TypeEnv -> TypeEnv
shadow to from = fmap (\\ unEnv to) from

haskellEnv :: TypeEnv
haskellEnv = foldl extend emptyEnv predefined
  where
    def :: String -> [Type] -> Typing
    def name tl = (Var name, TScheme [] (TArr tl))
    predefined = [ def "div" [TInt, TInt, TInt]
                 , def "mod" [TInt, TInt, TInt]
                 , def "not" [TBool, TBool]
                 ]

instance HasFreeVars TypeEnv where
  freevars e = nub $ concatMap (freevars .snd) (unEnv e)

-------------------------------------------------------------------------------- Substitution
type TSubst = (TVar, Type)      -- 型代入

-- | 型変数を型で置き換える型代入
subst :: TypeEnv -> [TSubst] -> TypeEnv
subst e l = foldl subst1 e l

subst1 :: TypeEnv -> TSubst -> TypeEnv
subst1 e (tv, ty) = fmap (map shaper) e
  where
    shaper (v, TScheme l t) = (v, TScheme (delete tv l) (lookdown t))
    lookdown :: Type -> Type
    lookdown t'@(TCon _) = t'
    lookdown t'@(TVar tv') = if tv' == tv then ty else t'
    lookdown (TArr l')     = TArr (map lookdown l')

data TypeError
  = UnificationFail Expr Type Type
  | InfiniteType Expr TVar Type
  | UnboundVariable Expr String
  | NotImplemented Expr
  deriving (Eq, Show)

instance PrettyPrint TypeError where
  prettyPrint (UnificationFail e t1 t2) =
    "The expression `" ++ prettyPrint e ++ " :: " ++ prettyPrint t1 ++ "` can`t unify with `" ++ prettyPrint t2 ++ "`."
  prettyPrint (InfiniteType e v t) =
    "The expression `" ++ prettyPrint e ++ " :: " ++ prettyPrint v ++ "` has an infinite type `" ++ prettyPrint t ++ "`."
  prettyPrint (UnboundVariable e v) =
    "The expression `" ++ prettyPrint e ++ "` contains an unbound variable `" ++ v ++ "`."
  prettyPrint (NotImplemented e) =
    "Sorry, we can't yet handle the expression `" ++ prettyPrint e ++ "`."

-- | 型を一致させる型代入を返す；なければ例外を起こす
unify :: Expr -> Type -> Type -> TypeEnv -> Infer TypeEnv
unify x t t' e
  | TVar v <- t, occurrenceCheck v t' = throwError (InfiniteType x v t')
  | Just u <- unifier t t'            = return $ subst e u
  | otherwise                         = throwError $ UnificationFail x t t'

unifier :: Type -> Type -> Maybe [TSubst]
-- Type Constant
unifier t1@(TCon _) t2@(TCon _)
  | t1 == t2 = Just []
  | otherwise = Nothing
unifier t@(TCon _) (TVar v)   = Just [(v, t)]
unifier (TCon _) _ = Nothing
unifier t1 t2@(TCon _)        = unifier t2 t1
-- Var
unifier t1@(TVar _) (TVar t2) = Just [(t2, t1)]
unifier (TVar t1) t2@(TArr _) = Just [(t1, t2)]
unifier t1 t2@(TVar _) = unifier t2 t1
-- Compound types
unifier (TArr l1) (TArr l2)
  | length l1 == length l2    = mconcat $ zipWith unifier l1 l2
  | otherwise = Nothing
-- unifier _ _ = Nothing

occurrenceCheck :: TVar -> Type -> Bool
occurrenceCheck t1 t2@(TArr _) = elem t1 (freevars t2)
occurrenceCheck _ _            = False

-------------------------------------------------------------------------------- Inference
type InferResult = Either TypeError (Type, TypeEnv)
type Infer a = StateT Int (ExceptT TypeError Identity) a

newTypeVar :: Infer TVar
newTypeVar = do { i <- (1 +) <$> get; put i; return $ TV i }

-- | 型環境e下における与えられた式の型を返す
-- | 型環境は部分式の評価において更新されるため、更新された型環境も返す必要がある。
infer :: Expr -> TypeEnv -> Infer (Type, TypeEnv)
infer (Lit (LInt _))    e = return (TInt, e)
infer (Lit (LBool _))   e = return (TBool, e)
infer (Lit (LString _)) e = return (TString, e)
infer (Ref v@(Var n))   e = case find ((v ==) . fst) (unEnv e) of
                              Just (_, TScheme _ t) -> return (t, e)
                              Nothing -> do t <- newTypeVar  -- t :: TVar
                                            let s = TScheme [t] (TVar t)
                                            return (TVar t, extend e (Var n, s))
infer (Paren x) e = infer x e
infer xp@(Op op x y) e0
  | elem op [Add, Sub, Mul]  = do (tx, e1) <- infer x e0
                                  (ty, e2) <- infer y =<< unify x tx TInt e1
                                  (TInt,) <$> unify y ty TInt e2
  | elem op [Eql] = do (tx, e1) <- infer x e0
                       (ty, e2) <- infer y e1
                       (TBool,) <$> unify xp tx ty e2
  | otherwise = throwError $ NotImplemented xp
infer (App xs) e0 = do
  let loop []    e ts = return (reverse ts, e)
      loop (a:l) e ts = do { (t', e') <- infer a e; loop l e' (t' : ts) }
  (ts, e1) <- loop xs e0 []
  r2 <- TArr . (\v -> tail ts ++ [v]) . TVar <$> newTypeVar
  (TArr l, e2) <- infer (head xs) =<< unify (head xs) (head ts) r2 e1
  case drop (length xs - 1) l of
    []  -> throwError $ UnificationFail (App xs) TUnit (TArr l)
    [e] -> return (e, e2)
    l'  -> return  (TArr l', e2)
infer x _ = throwError $ NotImplemented x

-------------------------------------------------------------------------------- interface
inferExpr :: Expr -> InferResult
inferExpr e = runIdentity $ runExceptT (evalStateT (infer e haskellEnv) 0)
