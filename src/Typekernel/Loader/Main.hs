module Typekernel.Loader.Main where
    import Typekernel.Loader.UEFI
    import Typekernel.Transpiler
    import Typekernel.Std.StringLiteral
    import Typekernel.Std.Log
    app :: UEFI ()
    app =do
        lit<-stringL "Warm welcome from Typekernel Typeboot!\n"
        putLog lit
    main :: C ()
    main=uefiMain app