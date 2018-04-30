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
 = TVar TVar
 | TCon Name
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

newtype TypeEnv = VarMap [(Var, Scheme)]
  deriving (Eq, Ord, Show)

extend :: TypeEnv -> (Var, Scheme) -> TypeEnv
extend (VarMap z) (x, s) = VarMap $ (x, s) : z

append :: TypeEnv -> TypeEnv -> TypeEnv
append (VarMap m1) (VarMap m2) = VarMap $ m1 ++ m2

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
      unify 
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

{-
e1 // v と e2 // v が unifiableならば
e'の下で、vに関するs1, s2がunfiableならば
(v, s') : e' を理由にこれらはunifiableである。
-}
unify :: TypeEnv -> TypeEnv -> Var -> Maybe TypeEnv
unify e1 e2 v
 | Just e' <- unifiable e1 e2 = case unifiable v s1 s2 e' of
                                  Just s' -> Just $ (v, s': e')
                                  _       -> Nothing
 | otherwise = Nothing
 where
   unifiable :: Var -> Scheme -> Scheme -> Maybe Scheme
   unifiable v s1 s2 = undefined

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

-}
