{-# LANGUAGE TemplateHaskell, TemplateHaskellQuotes #-}
-- TemplateHaskell helper functions.
module TH where
    import Language.Haskell.TH
    typeInstance :: Name->[Name]->Name->Dec
    typeInstance tf args result=TySynInstD tf (TySynEqn (map ConT args) (ConT result))

    promote :: a->Q a
    promote=return