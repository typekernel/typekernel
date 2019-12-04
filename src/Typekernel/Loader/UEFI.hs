{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies, UndecidableInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module Typekernel.Loader.UEFI where
    import Typekernel.Structure
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    import Control.Monad.Trans.Reader
    import Control.Monad.IO.Class
    import Control.Monad.Fix
    import Control.Monad.Trans.Class
    import Typekernel.Std.Basic
    import Typekernel.Std.Log
    import Data.Proxy
    import Typekernel.Std.StringLiteral
    import Typekernel.RAII
    import Typekernel.ProductType
    import Debug.Trace
    data UEFIAllocator'

    type UEFIAllocator=Typedef UEFIAllocator' (Product (Basic (Ptr UInt64)) (Basic UInt64))

    --newtype UEFIAllocator = UEFIAllocator {allocatorInfo :: Product (Basic (Ptr UInt64)) (Basic UInt64)}
    --type instance SizeOf UEFIAllocator = SizeOf (Product (Basic (Ptr UInt64)) (Basic UInt64))
    --instance (SizeOf UEFIAllocator ~ n) => Structure n UEFIAllocator where
    --    restore _=return . UEFIAllocator . Product
    currentPointer x = untypedef x >>= fstS
    allocatedPageSize x = untypedef x >>= sndS
    instance MemoryProvider UEFIAllocator where
        malloc' allocator sz = do
            addr<-(immUSize 0)
            mem<-(cast (Proxy::(Proxy (Ptr USize)))) addr
            let p=metadata mem
            --emit $ ""
            return mem
    data UEFIServices = UEFIServices {
        uefiAllocator :: UEFIAllocator
        --uefiTerminal :: 
    }

    -- UEFI monad, configured to use an UEFI heap.
    newtype UEFI a = UEFI {uefiToReader :: ReaderT UEFIServices C a} deriving (Monad, Functor, Applicative,  MonadFix)
    instance (MonadC UEFI) where
        liftC = UEFI . lift
    instance MonadHeap UEFI where
    --    malloc= asks (uefiAllocator 

    
    instance MonadLog UEFI where
        --logStringLiteral lit=liftC $ do
        --    emit $ "Print(L\"%a\", "++(metadata $ rawPointer lit)++");"
        --logString str=liftC $ do
        --    emit $ "Print(L\"%a\", "++(show str)++");"

    allocatePage :: UEFI UInt64
    allocatePage = do
        addr<-liftC $ immUInt64 0
        status<-liftC $ immUInt64 0
        --liftC $ emit $ (metadata status)++"=uefi_call_wrapper(SystemTable->BootServices->AllocatePages, 4, AllocateAnyPages,EfiLoaderData,1,&"++(metadata addr)++");"
        --liftC $ emit $ "Print(L\"Allocate status: %r %lx\\n\", "++(metadata status)++", "++(metadata addr)++");"
        return addr
    runUEFI :: UEFIServices->UEFI a->C a
    runUEFI s ma=runReaderT (uefiToReader ma) s
    uefiMain :: UEFI ()->C ()
    uefiMain uefi = do
        obj<-ensureObject "uefiMain"
        if obj then do
            let emit str=emitCDecl [str]
            emit "#include <efi.h>"
            emit "#include <efilib.h>"
            emit "EFI_SYSTEM_TABLE* GlobalST;"
            emit "extern uint64_t* __vectors[];"
            
            emit "EFI_STATUS EFIAPI efi_main (EFI_HANDLE ImageHandle, EFI_SYSTEM_TABLE* SystemTable)"
            emit "{"
            emit "    uint64_t typekernel_bootloader_main();"
            emit "    InitializeLib(ImageHandle, SystemTable);"
            emit "    GlobalST=SystemTable;"
            emit "    typekernel_bootloader_main();"
                
            emit "return EFI_SUCCESS;"
            emit "}"
            emit "void trap_handler(){Print(L\"Breakpoint hit in Typekernel!\\n\");asm volatile(\"cli;\");}"
            namedFunction "typekernel_bootloader_main" (\(x::Void)-> runRAII $ do
                    alloc<-construct (ctorNewtype $ ctorProd zeroBasic zeroBasic)
                    useScope alloc $ \allocator->liftC $ runUEFI (UEFIServices allocator) uefi
                    liftC $ immUInt64 0) 
            return ()
        else trace "Warning: uefiMain() called twice! ignoring..." $ return ()