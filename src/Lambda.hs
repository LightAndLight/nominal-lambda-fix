{-# language DeriveGeneric #-}
{-# language FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# language StandaloneDeriving, QuantifiedConstraints, UndecidableInstances #-}
module Lambda where

import Prelude hiding (id, (.), uncurry)

import Control.Category (id, (.))

import Data.Name
import GHC.Generics (Generic, Generic1)

import qualified Prelude

data LamF a
  = Var !Name
  | App a a
  | Lam (Tie Name a)
  deriving (Eq, Generic, Generic1, Show)
instance Permutable a => Permutable (LamF a)
instance Permutable1 LamF
instance Nominal a => Nominal (LamF a)
instance Nominal1 LamF
instance (Subst a Name, Subst a a) => Subst a (LamF a)

lamF :: Nom (Tie Name a) (LamF a)
lamF = nom_ Lam

appF :: Nom (a, a) (LamF a)
appF = nom_ (Prelude.uncurry App)

varF :: Nom Name (LamF a)
varF = nom_ Var

data Mu f = In { out :: f (Mu f) }

deriving instance (forall x. Show x => Show (f x)) => Show (Mu f)

mu :: Nom (f (Mu f)) (Mu f)
mu = nom_ In

instance Permutable1 f => Permutable (Mu f) where
  trans n1 n2 (In s) = In $ trans1 trans n1 n2 s
  perm p (In s) = In $ perm1 perm p s

instance Nominal1 f => Nominal (Mu f) where
  (#) n (In s) = sep1 (#) n s
  supp (In s) = supp1 supp s
  supply (In s) = supply1 supply s
  equiv (In s) = equiv1 equiv s

instance Subst1 e f => Subst e (Mu f) where
  subst m p (In s) = In $ subst1 subst m p s

id_ :: Nom () (Mu LamF)
id_ = mu . lamF . leftAdjunct (mu . varF . pi2)

const_ :: Nom () (Mu LamF)
const_ =
  mu . lamF .
  leftAdjunct (mu . lamF . leftAdjunct (mu . varF . pi1) . pi2)