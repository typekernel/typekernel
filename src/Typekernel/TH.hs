{-# LANGUAGE TemplateHaskell, TemplateHaskellQuotes #-}
-- TemplateHaskell helper functions.
module Typekernel.TH where
    import Language.Haskell.TH
    typeInstance :: Name->[Name]->Name->Dec
    typeInstance tf args result=TySynInstD tf (TySynEqn (map ConT args) (ConT result))

    classInstance :: [Name]->Dec
    classInstance args=InstanceD Nothing [] (foldl1 AppT $ map ConT args) []
    promote :: a->Q a
    promote=return

    generateNat :: Int->Name->Name->[Dec]
    generateNat maxn z s=
                let names=fmap mkName $ ["N"++(show x) | x<-[0..maxn]]
                    src=(PromotedT z):(fmap (\name->AppT (PromotedT s) (ConT name)) names)
                in zipWith (\a b->TySynD a [] b) names src