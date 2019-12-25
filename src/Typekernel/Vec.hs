{-# LANGUAGE DataKinds, KindSignatures, GADTs, FlexibleInstances, UndecidableInstances #-}
module Typekernel.Vec where
    import Typekernel.Nat
    import Data.Proxy
    import qualified Data.Vector as V
    
    data Vec a (n::Nat) = UnsafeMkVec {getVector :: V.Vector a}
    {- 
    data Vec a (n::Nat) where 
        Nil :: Vec a Z
        (:-) :: a->Vec a n -> Vec a (S n)

    infixr :-
    -}

    nilV :: Vec a 0
    nilV = UnsafeMkVec $ V.empty
    singletonV :: a->Vec a 1
    singletonV = UnsafeMkVec . V.singleton
    (-:) :: a->Vec a n->Vec a (NAdd 1 n)
    (-:) =concatV . singletonV

    infixr -:
    instance (Show a)=>Show (Vec a n) where
        show = show . toListV


    class VectorNat (n::Nat) where
        vectorNat' :: Proxy n->Vec Int n
    instance (KnownNat n)=>VectorNat n where
        vectorNat' p= UnsafeMkVec $ V.fromList [0..(natToInt p)-1]
    
    
    vectorNat :: (VectorNat n)=>Vec Int n
    vectorNat =vectorNat' Proxy
    
    mapV :: (a -> b) -> Vec a n -> Vec b n

    mapV f vec= UnsafeMkVec $ fmap f $ getVector vec


    toListV :: Vec a n->[a]

    toListV = V.toList . getVector

    sizeV :: Vec a n->Proxy n
    sizeV _=Proxy


    concatV :: Vec a m->Vec a n->Vec a (NAdd m n)
    concatV va vb= UnsafeMkVec $ (V.++) (getVector va) (getVector vb)

    (++:) = concatV
    
    mapMV :: (Monad m)=>(a->m b)->Vec a (n::Nat)->m (Vec b n)
    mapMV f vec=fmap UnsafeMkVec $ V.mapM f $ getVector vec