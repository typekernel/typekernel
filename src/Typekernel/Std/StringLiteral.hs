module Typekernel.Std.StringLiteral where
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    data StringLiteral = StringLiteral {rawPointer :: Ptr UInt8}

    stringL :: (MonadC m)=>String->m StringLiteral
    stringL str=liftC $ do
        name<-newIdent
        emitDecl $ "char "++name++"[] = "++(show str)++";"
        return $ StringLiteral $ Ptr name