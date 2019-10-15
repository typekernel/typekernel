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