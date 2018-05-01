{-# LANGUAGE
    DeriveFunctor
  , GeneralizedNewtypeDeriving
  #-}
-- http://dev.stephendiehl.com/fun/006_hindley_milner.html
-- https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter7/poly/src/Infer.hs

module Lib
    ( Expr(..)
    , runInfer
    , infer
    ) where

import Control.Monad
--import Control.Monad.Except
import Control.Monad.State

import Data.List

type Name = String

data Expr
  = Ref Var
  | FA Var [Expr]
  | App Expr Expr
  | Lit Lit
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
  deriving (Eq, Ord, Show)

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)

data Program = Program [Decl] Expr
  deriving Eq

type Decl = (String, Expr)

newtype TVar = TV Name
  deriving (Show, Eq, Ord)

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

-- x         => Forall [TV "x"] (TV "x")
-- f x = x   => Forall [TV "x"] (TArr [(TV "x"), (TV "x")])
-- f x y = y => Forall [TV "x", TV "y"] (TArr [(TV "x"), (TV "x"), (TV "x")])
data Scheme = Forall [TVar] Type
  deriving (Eq, Ord, Show)

newtype TypeEnv = VarMap { unEnv :: [(Var, Scheme)] }
  deriving (Eq, Ord, Show)

schemeOf :: TypeEnv -> Var -> Maybe Scheme
schemeOf e v = snd <$> find ((v ==) . fst) (unEnv e)

without :: TypeEnv -> Var -> TypeEnv
without (VarMap e) v = VarMap $ del e
  where
    del :: [(Var, Scheme)] -> [(Var, Scheme)]
    del [] = []
    del (x@(var, _):ns)
      | var == v  = del ns
      | otherwise = x : del ns

extend :: TypeEnv -> (Var, Scheme) -> TypeEnv
extend (VarMap z) (x, s) = VarMap $ (x, s) : z

append :: TypeEnv -> TypeEnv -> TypeEnv
append (VarMap m1) (VarMap m2) = VarMap $ m1 ++ m2

mergeSchemes :: Var -> Scheme -> Scheme -> Maybe Scheme
-- 1. identical patterns
mergeSchemes v (Forall _ (TCon tc)) (Forall _ (TCon tc'))  = Just $ Forall [] (TCon tc)
mergeSchemes v (Forall l1 t1@(TVar u1)) (Forall l2 t2@(TVar u2))
  | l1 == [u1] && l2 ==[u2]  = Just $ Forall l1 t1
  | null l1                  = Just $ Forall [] t1 -- free w.r.t. the scheme
  | otherwise                = Just $ Forall [] t2 -- free w.r.t. the scheme
-- ここで詰まった。
-- 操作としては以下の2種類を考えなければならない
-- * 変数名に対してスキーマを割り当てる、変数に対する別々のスキーマを融合する
-- * 型変数間の制約を解消する、型環境中の型変数を融合する
-- 後者は、ある型変数を*全て*のスキーマ中の型変数リストから削除し、
-- スキーマ中の型フィールド中のその型変数を別の型変数に置換することが必要だが、これができていない。
-- 型変数は複数のスキーマ中の型変数リストに出現する。
-- 従って、この関数は型変数名とその置き換えを返すべき。大域的な置換は受け取った側の作業。
-- * 型変数を型変数で置き換える： スキーマのリストから削除、スキーマの型を置換
-- * 型変数を構造型で置き換える： スキーマのリストから削除、スキーマの型を置換
-- どちらも処理としては同じ。また、処理の開始点はスキーマ間の融合で問題ない。
mergeSchemes v (Forall l1 (TList t1)) (Forall l2 (TList t2))
  | 
-- mergeSchemes v (TPair tp) (TPair tp') = 
-- mergeSchemes v (TArr ta) (TArr ta')   = 

-- 2. map to a single type variable
-- mergeSchemes v (TVar tv) (TCon tc)  = undefined
-- mergeSchemes v (TVar tv) (Tlist tl) = undefined
-- mergeSchemes v (TVar tv) (TPair tp) = undefined
-- mergeSchemes v (TVar tv) (TArr ta)  = undefined

-- 2. map to a single type variable (reflected)
-- mergeSchemes v (TCon tc)  (TVar tv) = undefined
-- mergeSchemes v (Tlist tl) (TVar tv) = undefined
-- mergeSchemes v (TPair tp) (TVar tv) = undefined
-- mergeSchemes v (TArr ta)  (TVar tv) = undefined

-- Otherwise, error
mergeSchemes v _ _ = Nothing
   

-- extend emptyEnv (Var "x", Forall [] typeInt)
emptyEnv :: TypeEnv
emptyEnv = VarMap []

-- Subst in a restricted 'TypeEnv' without 'Scheme'
type Subst = [(TVar, Type)]

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String

type Infer a = State Int a

-- runInfer :: Expr -> _
runInfer e = fst $ runState (infer e) 0

newTypeVar :: Infer TVar
newTypeVar = do i <- get
                let j = i + 1
                put j
                return $ TV ("(" ++ show j ++ ")")

infer :: Expr -> Infer (Scheme, TypeEnv)
infer (Ref (Var n))     = do t <- newTypeVar
                             let e = Forall [t] (TVar t)
                             return (e, [(n, t)])
infer (Lit (LInt _))    = return (Forall [] typeInt, emptyEnv)
infer (Lit (LBool _))   = return (Forall [] typeBool, emptyEnv)
infer (Lit (LString _)) = return (Forall [] typeChar, emptyEnv)
infer (Op Add x y)      = do (tx, ex) <- infer x
                             (ty, ey) <- infer y
                             let int = Forall [] typeInt
                             if tx == int && ty == int
                               then return (Forall [] typeInt, append ex ey)
                               else error (show (tx, ty))


-- * @e1 // v :: TypeEnv@ と @e2 // v :: TypeEnv@ が unifiableであり、
-- * e'の下で、vに関するs1, s2がunfiableならば、
-- (v, s') : e' を理由にこれらはunifiableである。
unify :: TypeEnv -> TypeEnv -> Maybe TypeEnv
unify e1 e2 = foldr (unifyOn e1 e2) (Just emptyEnv) vars
  where vars = nub $ (map fst (unEnv e1)) ++ (map fst (unEnv e2))

unifyOn :: TypeEnv -> TypeEnv -> Var -> Maybe TypeEnv -> Maybe TypeEnv
unifyOn _ _ _ Nothing = Nothing
unifyOn e1 e2 v (Just e) = subst v (schemeOf v e1) (schemeOf v e2) =<< unify (e1 `without` v) (e2 `without` v)
 where
   subst :: Var -> Scheme -> Scheme -> TypeEnv -> Maybe TypeEnv
   subst = undefined

{-
unify :: TypeEnv -> TVar -> Scheme -> Scheme -> Maybe Scheme
unify e t s@(Forall tl1 t1) (Forall tl2 t2) =

  | length tl1 /= length tl2 = Nothing
  | t1 == t2'                = Just s
  | Just merged <- merge t1 t2' = merged
    where typepair = zip tl1 tl2
          t2' = foldr (uncurry injectTypeVar) t2 typepair
          merge = undefined

injectTypeVar :: TVar -> TVar -> Type -> Type
injectTypeVar _ _ x@(TCon _) = x
injectTypeVar (TV s) d x@(TVar (TV t)) = if t == s then (TVar d) else x
injectTypeVar s d (TList t) = TList $ injectTypeVar s d t
injectTypeVar s d (TPair tl) = TPair $ map (injectTypeVar s d) tl
injectTypeVar s d (TArr tl) = TArr $ map (injectTypeVar s d) tl
-}

{-

  case (a, b) of
    (Forall _ (TVar _), Forall _ (TVar _))                -> Just s1
    (Forall _ (TCon n1), Forall _ (TCon n2)) | n1 == n2   -> Just s1
    (Forall _ (TCon _), Forall _ (TVar _))                -> Just s1
    (Forall _ (TVar _), Forall _ (TCon _))                -> Just s2
    (Forall _ (TList t1), Forall _ (TList t2)) | t1 == t2 -> Just s1
    (Forall _ (TArr a1 a2)

unifiable' :: TypeEnv -> TypeEnv -> Maybe TypeEnv
unifiable' (VarMap a) (VarMap b)
  | null comVs = Just $ VarMap (a ++ b)
  | elem Nothing uniVM = Nothing
    where
      varsA = map fst a
      varsB = map fst b
      comVs = intersect varsA varsB
      uniVM = map unify comVs
      compl s = [ a | a <- s, notElem (fst a) comVs ]
      uniMV name e1 e2 = case (s1, s2) of
        (Forall _ (TVar _), Forall _ (TVar _))                -> s1
        (Forall _ (TCon n1), Forall _ (TCon n2)) | n1 == n2   -> s1
        (Forall _ (TCon _), Forall _ (TVar _))                -> s1
        (Forall _ (TVar _), Forall _ (TCon _))                -> s2
        (Forall _ (TList t1), Forall _ (TList t2)) | t1 == t2 -> s1
        (Forall _ (TArr a1 a2)
          , Forall _ (TArr b1 b2)) | a1 == b1 && a2 == b2     -> s1
        where
          (Just (_, s1)) = find name e1
          (Just (_, s2)) = find name e2
  
x = runInfer (Op Add (Lit (LInt 3)) (Ref (Var "x")))
-}
