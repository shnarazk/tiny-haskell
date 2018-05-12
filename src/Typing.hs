{-# LANGUAGE
    FlexibleInstances
  , PatternSynonyms
  , TupleSections
  , TypeSynonymInstances
  , ViewPatterns
  #-}
-- http://dev.stephendiehl.com/fun/006_hindley_milner.html
-- https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter7/poly/src/Infer.hs

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
import Data.Maybe
import AST

-------------------------------------------------------------------------------- Type system
class HasFreeVars s where
  freevars :: s -> [TVar]

newtype TVar = TV Int
  deriving (Eq, Ord, Show)

instance PrettyPrint TVar where
  precedenceOf _ = 0
  pp _ (TV name) = "t" ++ show name

data Type
 = TCon Name                    -- Constant
 | TVar TVar                    -- Variable
 | TLst Type                    -- List
 | TTpl [Type]                  -- Tuple
 | TArr [Type]                  -- Arror
 deriving (Eq, Ord, Show)

instance PrettyPrint Type where
  precedenceOf (TCon c) = maxBound
  precedenceOf (TVar t) = maxBound
  precedenceOf (TLst l) = maxBound
  precedenceOf (TTpl l) = maxBound
  precedenceOf (TArr l) = 0
  pp n (TCon c) = c
  pp n (TVar t) = pp n t
  pp _ (TLst l) = "[" ++ pp 0 l ++ "]"
  pp _ (TTpl l) = "(" ++ intercalate ", " (map (pp 0) l) ++ ")"
  pp n (TArr l) = intercalate " -> " (map (enclose (n + 1)) l)

instance HasFreeVars Type where
  freevars (TCon _)  = []
  freevars (TVar x)  = [x]
  freevars (TLst t) = freevars t
  freevars (TTpl l) = nub $ concatMap freevars l
  freevars (TArr l)  =  nub $ concatMap freevars l

pattern TInt    :: Type
pattern TInt    = TCon "Int"
pattern TBool   :: Type
pattern TBool   = TCon "Bool"
pattern TChar   :: Type
pattern TChar   = TCon "Char"
pattern TString :: Type
pattern TString = TLst TChar
pattern TUnit   :: Type
pattern TUnit   = TCon "()"

tVarsIn :: Type -> [TVar]
tVarsIn (TCon _) = []
tVarsIn (TVar v) = [v]
tVarsIn (TLst t) = tVarsIn t
tVarsIn (TTpl l) = nub $ concatMap tVarsIn l
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
  precedenceOf _ = 0
  pp _ (TScheme [] t) = "S:" ++ pp 0 t
  pp _ (TScheme vs t) = "S" ++ intercalate "." (map (pp 0) (sort vs)) ++ "." ++ pp 0 t

-------------------------------------------------------------------------------- TypeEnv
type Typing = (Var, TScheme)

newtype Tagged a = Tagged { unWrap :: a }
  deriving (Eq, Ord, Show)

instance Functor Tagged where
  fmap f = Tagged . f . unWrap

type TypeEnv = Tagged [Typing]

instance PrettyPrint TypeEnv where
  precedenceOf _ = 0
  pp _ (reorderTypeVars -> unEnv -> env)
    | null env  = "empty environment"
    | otherwise = "E{" ++ intercalate ", " (map f (sort env)) ++ "}"
    where f (v, TScheme [] t) = pp 0 v ++ " :: " ++ pp 0 t
          f (v, s) = pp 0 v ++ " :: " ++ pp 0 s

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
    lookdown (TLst t')     = TLst (lookdown t')
    lookdown (TTpl l')     = TTpl (map lookdown l')
    lookdown (TArr l')     = TArr (map lookdown l')

data TypeError
  = UnificationFail Expr Type Type
  | InfiniteType Expr TVar Type
  | UnboundVariable Expr String
  | NotImplemented Expr
  deriving (Eq, Show)

instance PrettyPrint TypeError where
  precedenceOf _ = 0
  pp _ (UnificationFail e t1 t2) =
    "The expression `" ++ pp 0 e ++ " :: " ++ pp 0 t1 ++ "` can`t unify with `" ++ pp 0 t2 ++ "`."
  pp _ (InfiniteType e v t) =
    "The expression `" ++ pp 0 e ++ " :: " ++ pp 0 v ++ "` has an infinite type `" ++ pp 0 t ++ "`."
  pp _ (UnboundVariable e v) =
    "The expression `" ++ pp 0 e ++ "` contains an unbound variable `" ++ v ++ "`."
  pp _ (NotImplemented e) =
    "Sorry, we can't yet handle the expression `" ++ pp 0 e ++ "`."

-- | 型を一致させる型代入を求める
-- なければ例外を起こす
unify :: Expr -> Type -> Type -> TypeEnv -> Infer TypeEnv
unify x t t' e
  | TVar v <- t, occurrenceCheck v t' = throwError (InfiniteType x v t')
  | Just u <- unifier t t'            = return $ subst e u
  | otherwise                         = throwError $ UnificationFail x t t'
--    case unifier t t' of
--      Just u -> return $ subst e u
--      Nothing -> throwError (UnificationFail x t t')

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
unifier (TVar t1) t2@(TLst _) = Just [(t1, t2)]
unifier (TVar t1) t2@(TTpl _) = Just [(t1, t2)]
unifier (TVar t1) t2@(TArr _) = Just [(t1, t2)]
unifier t1 t2@(TVar _) = unifier t2 t1
-- Compound types
unifier (TLst t1) (TLst t2)   = unifier t1 t2
unifier (TTpl l1) (TTpl l2)
  | length l1 == length l2    = mconcat $ zipWith unifier l1 l2
  | otherwise = Nothing
unifier (TArr l1) (TArr l2)
  | length l1 == length l2    = mconcat $ zipWith unifier l1 l2
  | otherwise = Nothing
unifier _ _ = Nothing

occurrenceCheck :: TVar -> Type -> Bool
occurrenceCheck t1 t2@(TLst _) = elem t1 (freevars t2)
occurrenceCheck t1 t2@(TTpl _) = elem t1 (freevars t2)
occurrenceCheck t1 t2@(TArr _) = elem t1 (freevars t2)
occurrenceCheck _ _            = False

-------------------------------------------------------------------------------- Inference
type InferResult = Either TypeError (Type, TypeEnv)
type Infer a = StateT Int (ExceptT TypeError Identity) a

newTypeVar :: Infer TVar
newTypeVar = do { i <- (1 +) <$> get; put i; return $ TV i }

-- | 型環境e下における与えられた式の型を返す
-- | 型環境は部分式の評価において更新されるため、更新された型環境も返す必要がある。
-- | この関数は失敗しない。例外を返すのはunifyのみ
infer :: Expr -> TypeEnv -> Infer (Type, TypeEnv)
infer (Lit (LInt _))    e = return (TInt, e)
infer (Lit (LBool _))   e = return (TBool, e)
infer (Lit (LString _)) e = return (TString, e)
infer (Ref v@(Var n))   e = case find ((v ==) . fst) (unEnv e) of
                              Just (_, TScheme _ t) -> return (t, e)
                              Nothing -> throwError $ UnboundVariable (Ref v) n
--                              Nothing -> do t <- newTypeVar  -- t :: TVar
--                                            let s = TScheme [t] (TVar t)
--                                            return (TVar t, extend e (Var n, s))
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
    l'  -> return  (TArr l', e2)
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
  t <- newTypeVar  -- t :: TVar
  let s = TScheme [t] (TVar t)
  (t1, e1) <- infer x1 e0                         -- x1からvはt1型である（自由変数が消えるunifyは不可）
  (_ , e2) <- infer x2 (extend e1 (v,s))          -- 最初から自由変数がなければ消えたりはしない
  let (Just (TScheme _ tv)) = schemeOf e2 v       -- x2での型推論よりvの型はtvでなければならない
  if null . intersect (tVarsIn t1) $ freevars e2  -- スキーマ変数が自由変数に含まれない
    then do e3 <- unify (Ref v) t1 tv e2          -- vに関する型t1, tvをunify
            (tl, e4) <- infer x2 e3
            return (tl, fmap (filter ((v /=) . fst)) e4)
    else throwError $ UnificationFail (Ref v) t1 tv
--
infer (Decl v args x) e0 = do
  tvs <- mapM (const newTypeVar) (v:args)         -- = arg types : return type
  let argSchemes = map (\t -> TScheme [] (TVar t)) (init tvs)
  let vScheme = TScheme tvs (TArr (map TVar tvs))
  let e1 = foldl extend e0 $ zip (v:args) (vScheme:argSchemes)
  (t, e2) <- infer x e1
  e3 <- unify (Ref v) (TVar (last tvs)) t e2      -- eとvの返値型は同じ
  let e4 = fmap (filter ((`notElem` args) . fst)) e3
  return (derived . fromJust $ schemeOf e4 v, e4)
infer x _ = throwError $ NotImplemented x

-------------------------------------------------------------------------------- interface
inferExpr :: Expr -> InferResult
inferExpr e = runIdentity $ runExceptT (evalStateT (infer e haskellEnv) 0)

reorderTypeVars :: TypeEnv -> TypeEnv
reorderTypeVars (unEnv -> l) = makeEnv $ map (\(v, s) -> (v, replace s)) l
  where ordering = zip (nub . sort $ concatMap (tVarsIn . derived . snd) l) (map TV [1..])
        to :: TVar -> TVar
        to v
          | Just (_, v') <- find ((v ==) . fst) ordering = v'
          | otherwise = v
        rep :: Type -> Type
        rep t@(TCon _) = t
        rep (TVar v)   = TVar (to v)
        rep (TLst t)   = TLst (rep t)
        rep (TTpl l)   = TTpl (map rep l)
        rep (TArr l)   = TArr (map rep l)
        replace :: TScheme -> TScheme
        replace (TScheme fvs typ) = TScheme (map to fvs) (rep typ)
