module Typekernel.Std.StringLiteral where
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    import Typekernel.IR
    data StringLiteral = StringLiteral {rawPointer :: Ptr UInt8}

    stringL :: (MonadC m)=>String->m StringLiteral
    stringL str=liftC $ do
        name<-newIdent
        emitIRTop $ IRStringLiteral name str
        return $ StringLiteral $ Ptr name