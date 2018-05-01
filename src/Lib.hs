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
    , typeInt
    , typeBool
    , typeChar
    , TScheme(..)
    , Typing
    , TSubst
    , TypeEnv(..)
    , emptyEnv
    , tySubst
    , schemeOf
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

-- x         => TScheme [TV "x"] (TV "x")
-- f x = x   => TScheme [TV "x"] (TArr [(TV "x"), (TV "x")])
-- f x y = y => TScheme [TV "x", TV "y"] (TArr [(TV "x"), (TV "x"), (TV "x")])
data TScheme = TScheme [TVar] Type
  deriving (Eq, Ord, Show)

type Typing = (Var, TScheme)
type TSubst = (TVar, Type)      -- 型代入

newtype TypeEnv = VarMap { unEnv :: [Typing] }
  deriving (Eq, Ord, Show)

schemeOf :: TypeEnv -> Var -> Maybe TScheme
schemeOf e v = snd <$> find ((v ==) . fst) (unEnv e)

without :: TypeEnv -> Var -> TypeEnv
without (VarMap e) v = VarMap $ del e
  where
    del :: [(Var, TScheme)] -> [(Var, TScheme)]
    del [] = []
    del (x@(var, _):ns)
      | var == v  = del ns
      | otherwise = x : del ns

extend :: TypeEnv -> Typing -> TypeEnv
extend (VarMap z) (x, s) = VarMap $ (x, s) : z

append :: TypeEnv -> TypeEnv -> TypeEnv
append (VarMap m1) (VarMap m2) = VarMap $ m1 ++ m2

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

-- | 型変数を型で置き換える型代入
-- >>> t = VarMap [(Var "x", TScheme [] (TCon "Int")), (Var "y", TScheme [TV "1"] (TVar (TV "1")))]
-- >>> tySubst t (TV "1", TCon "Int")
-- VarMap {unEnv = [(Var "x",TScheme [] (TCon "Int")),(Var "y",TScheme [] (TCon "Int"))]}
-- >>> tySubst t (TV "2", TCon "Int")
-- VarMap {unEnv = [(Var "x",TScheme [] (TCon "Int")),(Var "y",TScheme [TV "1"] (TVar (TV "1")))]}
-- >>> tySubst t (TV "1", TV "2")
-- VarMap {unEnv = [(Var "x",TScheme [] (TCon "Int")),(Var "y",TScheme [TV "1"] (TVar (TV "1")))]}
tySubst :: TypeEnv -> TSubst -> TypeEnv
tySubst e (tv, ty) = VarMap . map shaper $ unEnv e
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

-- ここまで

{-
mergeSchemes :: Var -> TScheme -> TScheme -> Maybe TScheme
-- 1. identical patterns
mergeSchemes v (TScheme _ (TCon tc)) (TScheme _ (TCon tc'))  = Just $ TScheme [] (TCon tc)
mergeSchemes v (TScheme l1 t1@(TVar u1)) (TScheme l2 t2@(TVar u2))
  | l1 == [u1] && l2 ==[u2]  = Just $ TScheme l1 t1
  | null l1                  = Just $ TScheme [] t1 -- free w.r.t. the scheme
  | otherwise                = Just $ TScheme [] t2 -- free w.r.t. the scheme
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
mergeSchemes v (TScheme l1 (TList t1)) (TScheme l2 (TList t2))
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
-}

-- extend emptyEnv (Var "x", TScheme [] typeInt)
emptyEnv :: TypeEnv
emptyEnv = VarMap []

-- Subst in a restricted 'TypeEnv' without 'TScheme'
-- type Subst = [(TVar, Type)]

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

infer :: Expr -> Infer (TScheme, TypeEnv)
infer (Ref (Var n))     = do t <- newTypeVar  -- t :: TVar
                             let e = TScheme [t] (TVar t) :: TScheme
                             return (e, VarMap [(Var n, e)])
infer (Lit (LInt _))    = return (TScheme [] typeInt, emptyEnv)
infer (Lit (LBool _))   = return (TScheme [] typeBool, emptyEnv)
infer (Lit (LString _)) = return (TScheme [] typeChar, emptyEnv)
infer (Op Add x y)      = do (tx, ex) <- infer x
                             (ty, ey) <- infer y
                             let int = TScheme [] typeInt
                             if tx == int && ty == int
                               then return (TScheme [] typeInt, append ex ey)
                               else error (show (tx, ty))


-- * @e1 // v :: TypeEnv@ と @e2 // v :: TypeEnv@ が unifiableであり、
-- * e'の下で、vに関するs1, s2がunfiableならば、
-- (v, s') : e' を理由にこれらはunifiableである。
unify :: TypeEnv -> TypeEnv -> Maybe TypeEnv
unify e1 e2 = foldr (unifyOn e1 e2) (Just emptyEnv) vars
  where vars = nub $ (map fst (unEnv e1)) ++ (map fst (unEnv e2))

unifyOn :: TypeEnv -> TypeEnv -> Var -> Maybe TypeEnv -> Maybe TypeEnv
unifyOn _ _ _ Nothing = Nothing
unifyOn e1 e2 v (Just e)
  | Just s1 <- schemeOf e1 v, Just s2 <- schemeOf e2 v = subst v s1 s2 =<< unify (e1 `without` v) (e2 `without` v)
 where
   subst :: Var -> TScheme -> TScheme -> TypeEnv -> Maybe TypeEnv
   subst = undefined

{-
unify :: TypeEnv -> TVar -> TScheme -> TScheme -> Maybe TScheme
unify e t s@(TScheme tl1 t1) (TScheme tl2 t2) =

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
    (TScheme _ (TVar _), TScheme _ (TVar _))                -> Just s1
    (TScheme _ (TCon n1), TScheme _ (TCon n2)) | n1 == n2   -> Just s1
    (TScheme _ (TCon _), TScheme _ (TVar _))                -> Just s1
    (TScheme _ (TVar _), TScheme _ (TCon _))                -> Just s2
    (TScheme _ (TList t1), TScheme _ (TList t2)) | t1 == t2 -> Just s1
    (TScheme _ (TArr a1 a2)

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
        (TScheme _ (TVar _), TScheme _ (TVar _))                -> s1
        (TScheme _ (TCon n1), TScheme _ (TCon n2)) | n1 == n2   -> s1
        (TScheme _ (TCon _), TScheme _ (TVar _))                -> s1
        (TScheme _ (TVar _), TScheme _ (TCon _))                -> s2
        (TScheme _ (TList t1), TScheme _ (TList t2)) | t1 == t2 -> s1
        (TScheme _ (TArr a1 a2)
          , TScheme _ (TArr b1 b2)) | a1 == b1 && a2 == b2     -> s1
        where
          (Just (_, s1)) = find name e1
          (Just (_, s2)) = find name e2

x = runInfer (Op Add (Lit (LInt 3)) (Ref (Var "x")))
-}
