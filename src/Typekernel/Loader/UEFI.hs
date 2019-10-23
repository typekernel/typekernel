module Typekernel.Loader.UEFI where
    import Typekernel.Structure
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    data UEFIAllocator
    data UEFIServices = UEFIServices {
        uefiAllocator :: UEFIAllocator
        --uefiTerminal :: 
    }

    -- UEFI monad, configured to use an UEFI heap.
    data UEFI a
    uefiMain :: UEFI ()->C ()
    uefiMain uefi = do
        emit "#include <efi.h>"
        emit "#include <efilib.h>"
        emit "EFI_STATUS EFIAPI efi_main (EFI_HANDLE ImageHandle, EFI_SYSTEM_TABLE* SystemTable)"
        emit "{"
        indented $ do
                --f undefined
                emit "return EFI_SUCCESS;"
        emit "}"