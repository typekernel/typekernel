{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies, DataKinds, TemplateHaskell, UndecidableInstances, DeriveFunctor, ScopedTypeVariables, RecursiveDo #-}
import Data.Maybe
import Data.Proxy
newtype Ptr = Ptr {toInt :: Integer}
data Nat=Z|S Nat

--type family Sized a :: Nat
data UInt8
--type instance Sized UInt8=S Z
class Sized t (s::Nat) | t->s
class Option' a where
    data Option a
    --isSome :: Option a->Bool

data Pack a

instance (Sized a s)=>Option' (Pack a) where
    data Option (Pack a)=Proxy (S s)

instance (Sized a s)=>Sized (Option (Pack a)) (S s)