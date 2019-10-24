{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies, UndecidableInstances, MultiParamTypeClasses #-}
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
            emit $ ""
            return mem
    data UEFIServices = UEFIServices {
        uefiAllocator :: UEFIAllocator
        --uefiTerminal :: 
    }

    -- UEFI monad, configured to use an UEFI heap.
    newtype UEFI a = UEFI {uefiToReader :: ReaderT UEFIServices C a} deriving (Monad, Functor, Applicative, MonadIO, MonadFix)
    instance (MonadC UEFI) where
        liftC = UEFI . lift
    instance MonadHeap UEFI where
    --    malloc= asks (uefiAllocator 

    instance MonadLog UEFI where
        logStringLiteral lit=liftC $ do
            emit $ "Print(L\"%a\", "++(metadata $ rawPointer lit)++");"
    runUEFI :: UEFIServices->UEFI a->C a
    runUEFI s ma=runReaderT (uefiToReader ma) s
    uefiMain :: UEFI ()->C ()
    uefiMain uefi = do
        emit "#include <efi.h>"
        emit "#include <efilib.h>"
        emit "EFI_STATUS EFIAPI efi_main (EFI_HANDLE ImageHandle, EFI_SYSTEM_TABLE* SystemTable)"
        emit "{"
        indented $ do
                emit "InitializeLib(ImageHandle, SystemTable);"
                runRAII $ do
                    alloc<-construct (ctorNewtype $ ctorProd zeroBasic zeroBasic)
                    useScope alloc $ \allocator->liftC $ runUEFI (UEFIServices allocator) uefi
                   
                    
                emit "return EFI_SUCCESS;"
        emit "}"