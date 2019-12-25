{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Typekernel.Kernel.Main where
    import Typekernel.Transpiler
    import Typekernel.Std.StringLiteral
    import Typekernel.Std.Log
    import Typekernel.Std.X86_64
    import Typekernel.C4mAST
    import Typekernel.Constant
    import Typekernel.Std.Vec
    import Typekernel.Structure
    import Data.Proxy
    import Typekernel.Nat
    import Typekernel.Vec
    import Text.RawString.QQ
    import Typekernel.Bound
    {-
    app :: UEFI ()
    app =do
        lit<-stringL "Warm welcome from Typekernel Typeboot!\n"
        --[logF|My \{warm "welcome"\} is: {}|] lit
        pg_gdt<-allocatePage
        pg_gdt<-liftC $ cast (Proxy :: Proxy (Ptr UInt64)) pg_gdt
        pg_idt<-allocatePage 
        pg_idt<-liftC $ cast (Proxy :: Proxy (Ptr UInt64)) pg_idt
        mem_state<-allocatePage
        mem_state<-liftC $ cast (Proxy :: Proxy (Ptr UInt64)) mem_state
        let memgdt=Memory pg_gdt
        let memidt=Memory pg_idt
        let idt_table=mapV (\x-> (vectoredIDTItem x False Ring0)) $ vectorNat' (Proxy :: Proxy N256)
        let idt_ctor=ctorNewtype (ctorArray idt_table)
        initializeX86_64 memgdt memidt idt_ctor (Memory mem_state)
        --[logF|x86_64 Environment prepared. Now triggering a breakpoint...{}|] "\n"
        --liftC $ emit $ "asm volatile (\"int $0x03\");"
        --liftC $ emit "asm volatile(\"cli;\");"
        [logF|returning from handler{}|] "\n"
        --liftC $ emit "asm volatile(\"cli;\");"
        --liftC $ emit "asm volatile(\"hlt;\");"
        --liftC $ emit $ "asm volatile (\"int $0x03\");"
        [logF|returning from handler{}|] "\n"
        --liftC $ emit "asm volatile(\"cli;\");"
        --liftC $ emit $ "asm volatile (\"int $0x03\");"
        [logF|returning from handler{}|] "\n"
        --liftC $ emit "asm volatile(\"cli;\");"
        --liftC $ emit $ "asm volatile (\"int $0x03\");"
        [logF|returning from handler{}|] "\n"
        --liftC $ emit "asm volatile(\"cli;\");"

        return ()
    -}

    kernel_hardcode :: String
    kernel_hardcode = [r|
        usize_t kentry();
        uint8_t gdt_page[4096];
        uint8_t idt_page[4096];
        void _kentry(){
            kentry();
        }
    |]
    kernel :: C()->C ()
    kernel fn = do
        onceC "kernel" $ emitCDecl [kernel_hardcode]
        --namedFunction :: (FirstClass b, FirstClassList a, MonadC m)=>String->(a->m b)->m (Fn a b)
        kmain<-namedFunction "kmain" (\(_ :: Void)->do
                fn
                immUSize 0)
        kentry<-namedFunction "kentry" (\(_ :: Void)->do
            invoke kmain Void
            immUSize 0)
                
        return ()
    
    main :: C ()
    main=kernel $ do
        foreverLoop $ return () -- infiloop
        return ()