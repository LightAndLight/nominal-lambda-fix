{-# language DeriveGeneric #-}
{-# language FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Lambda where

import Data.Name.Class (Permutable(..), Nominal(..), Permutable1(..), Nominal1(..))
import Data.Name.Substitution (Subst(..))
import Data.Name.Tie (Tie)
import Data.Name.Type (Name)
import GHC.Generics (Generic)

data LamF a
  = Var !Name
  | App a a
  | Lam (Tie Name a)
  deriving (Eq, Generic, Show)
instance Permutable a => Permutable (LamF a)
instance Nominal a => Nominal (LamF a)
instance (Subst a Name, Subst a a) => Subst a (LamF a)

data Mu f = In { out :: f (Mu f) }

instance Permutable1 f => Permutable (Mu f) where
  trans n1 n2 (In s) = In $ trans1 trans n1 n2 s
  perm p (In s) = In $ perm1 perm p s

instance Nominal1 f => Nominal (Mu f) where
  (#) n (In s) = _
  supp (In s) = _
  supply (In s) = _
  equiv (In s) n1 n2 = _
