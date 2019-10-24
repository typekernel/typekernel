module Typekernel.Std.StringLiteral where
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    data StringLiteral = StringLiteral {rawPointer :: Ptr UInt8}

    stringL :: String->C StringLiteral
    stringL str=do
        name<-newIdent
        emitDecl $ "char "++name++"[] = "++(show str)++";"
        return $ StringLiteral $ Ptr name