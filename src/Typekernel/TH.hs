{-# LANGUAGE TemplateHaskell, TemplateHaskellQuotes #-}
-- TemplateHaskell helper functions.
module Typekernel.TH where
    import Language.Haskell.TH
    import Control.Monad
    typeInstance :: Name->[Name]->Name->Dec
    typeInstance tf args result=TySynInstD tf (TySynEqn (map ConT args) (ConT result))

    classInstance :: [Name]->Dec
    classInstance args=InstanceD Nothing [] (foldl1 AppT $ map ConT args) []
    promote :: a->Q a
    promote=return

    generateLegacyNat :: Int->Name->Name->[Dec]
    generateLegacyNat maxn z s=
                let names=fmap mkName $ ["N"++(show x) | x<-[0..maxn]]
                    src=(PromotedT z):(fmap (\name->AppT (PromotedT s) (ConT name)) names)
                in zipWith (\a b->TySynD a [] b) names src
    generateNat :: Int->[Dec]
    generateNat maxn=
                let names=fmap mkName $ ["N"++(show x) | x<-[0..maxn]]
                    src=[LitT (NumTyLit x) | x<-[0..(fromIntegral maxn)]]
                in zipWith (\a b->TySynD a [] b) names src

    
    
    
    -- | Declares a constant `String` function with a given name
    -- that returns a random string generated on compile time.
    createToken :: String -> DecsQ
    createToken fname = liftM (: []) $
        funD (mkName fname) [clause [] (normalB $ litE $ stringL fname) []]

