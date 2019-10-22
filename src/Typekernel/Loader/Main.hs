module Typekernel.Loader.Main where
    import Typekernel.Loader.UEFI

    app uefi=do
        return ()
    main=uefiMain app 