module Typekernel.Loader.UEFI where
    import Typekernel.Structure
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    data UEFIAllocator
    data UEFIServices = UEFIServices {
        uefiAllocator :: UEFIAllocator
        
    }

    uefiMain :: (UEFIServices->C ())->C ()
    uefiMain f = do
        emit "#include <efi.h>"
        emit "#include <efilib.h>"
        emit "EFI_STATUS EFIAPI efi_main (EFI_HANDLE ImageHandle, EFI_SYSTEM_TABLE* SystemTable)"
        emit "{"
        indented $ do
                f undefined
                emit "return EFI_SUCCESS;"
        emit "}"