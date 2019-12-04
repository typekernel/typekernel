{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}
module Typekernel.Vec where
    import Typekernel.Nat
    import Data.Proxy
    data Vec a (n::Nat) where 
        Nil :: Vec a Z
        (:-) :: a->Vec a n -> Vec a (S n)

    infixr :-


    instance (Show a)=>Show (Vec a n) where
        show = show . toListV

    class VectorNat (n::Nat) where
        vectorNat :: Vec Int n
    instance VectorNat Z where
        vectorNat =Nil
    instance (VectorNat n)=>VectorNat (S n) where
        vectorNat =0:-(mapV (+1) vectorNat)
    
    vectorNat' :: (VectorNat n)=>Proxy n->Vec Int n
    vectorNat' _=vectorNat
    mapV :: (a -> b) -> Vec a n -> Vec b n

    mapV _ Nil=Nil
    mapV f (x:-xs)=(f x):-(mapV f xs)


    toListV :: Vec a n->[a]

    toListV Nil=[]
    toListV (x:-xs)=x:(toListV xs)

    sizeV :: Vec a n->Proxy n
    sizeV _=Proxy


    concatV :: Vec a m->Vec a n->Vec a (NAdd m n)
    concatV Nil xs = xs
    concatV (x:-xs) ys = x:-(concatV xs ys)

    (++:) = concatV
    
    mapMV :: (Monad m)=>(a->m b)->Vec a n->m (Vec b n)
    mapMV f Nil=return Nil
    mapMV f (x:-xs) = do
        mx<-f x
        mxs<-mapMV f xs
        return $ mx:-mxs