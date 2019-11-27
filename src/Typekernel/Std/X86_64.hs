{-# LANGUAGE DataKinds, PolyKinds, NumericUnderscores, ScopedTypeVariables, FlexibleContexts, TypeFamilies #-}
module Typekernel.Std.X86_64 where
    import Typekernel.Structure
    import Typekernel.ProductType
    import Typekernel.Nat
    import Typekernel.Memory
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    import Typekernel.MLens
    import Typekernel.Constant
    import Typekernel.Std.Basic
    import Typekernel.Std.Vec
    import Typekernel.Std.Box
    import Data.Bits
    import Data.Proxy

    -- TODO: a beautiful compile-time evaluation.
    kcode =0x0020980000000000
    ucode =0x0020F80000000000
    kdata=0x0000920000000000
    udata=0x0000F20000000000
    ucode32=0x00cffa00_0000ffff
    udata32=0x00cff200_0000ffff



    -- genGDTTable = dataSeg $ (singleDword 0) ++: kcode ++: ucode ++: kdata ++: udata ++: ucode32 ++: udata32 ++: (singleDword 0)

    newtype IDTHandler = IDTHandler {handlePtr :: UInt64}
    --idtHandler :: (MonadC env)=>MLens env IDTItem IDTHandler
    --idtHandler=mkLens getter setter where
    --    getter 
    data GDTItem' 
    type GDTItem = Typedef GDTItem' (Basic UInt64)
    data GDTTable' (s::Nat)

    -- The first of which is empty
    type GDTTable (s::Nat) = Typedef (GDTTable' s) (Array (S s) GDTItem)

    -- Given tss value, yield ctor.
    -- Well the signature become unreadable...
    tssToGDT::(MonadC m)=>UInt64->Constructor m (GDTTable N7)
    tssToGDT tss mem=do
        gdtimm<-mapMV (liftC . immUInt64 . fromIntegral) (0:-kcode:-ucode:-kdata:-udata:-ucode32:-udata32:-Nil)
        let initializer=(mapV (\(ui::UInt64)->liftC . (ctorNewtype $ ctorBasic ui)) (gdtimm ++: (tss :- Nil))) 
        ctorNewtype (ctorArray initializer) mem

    data IDTItem'
    type IDTItem = Typedef IDTItem' (Product (Basic UInt64) (Basic UInt64))
    data IDTTable'
    type IDTTable = Typedef IDTTable' (Array N7 IDTItem)

    emptyIDTItem :: (MonadC m)=>Constructor m IDTItem
    emptyIDTItem mem = liftC $ ctorNewtype (ctorProd zeroBasic zeroBasic) mem

    data Ring = Ring0 | Ring1 | Ring2 | Ring3
    ringToInt :: Ring->Int
    ringToInt Ring0=0
    ringToInt Ring1=1
    ringToInt Ring2=2
    ringToInt Ring3=3

    vectoredIDTItem :: (MonadC m)=>Int->Bool->Ring->Constructor m IDTItem
    vectoredIDTItem index isintr ring mem = liftC $ do
        addr<-immUInt64 0
        --emit $ "EFI_LOADED_IMAGE *loaded_image = NULL;"
        --emit $ "uefi_call_wrapper(SystemTable->BootServices->HandleProtocol,3, ImageHandle,&LoadedImageProtocol,(void **)&loaded_image);"
        emit $ "extern uint64_t vector"++(show index)++";"
        emit $ (metadata addr) ++" = ((uint64_t)(&vector"++(show index)++"));"
        ist<-immUInt64 0
        exist<-immUInt64 0x800000000000
        r<-immUInt64 $ fromIntegral ((ringToInt ring) `shiftL` 45)
        selector<-immUInt64 (1*8*7)
        lower<-immUInt64 0
        mask<-immUInt64 0xffff
        offset_1<-binary opAnd addr mask
        lower<-binary opOr lower offset_1
        selector_shamt<-immUInt8 16
        selector<-binary opLShift selector selector_shamt
        lower<-binary opOr lower selector
        trapflag<-immUInt64 0xf0000000000
        intrflag<-immUInt64 0xe0000000000
        lower<-if isintr then binary opOr lower intrflag else binary opOr lower trapflag
        lower<-binary opOr lower exist
        lower<-binary opOr lower r
        offset2_shamt<-immUInt8 32
        offset2<-binary opLShift addr offset2_shamt
        mask<-immUInt64 0xffff000000000000
        offset2<-binary opAnd offset2 mask
        lower<-binary opOr lower offset2
        mset (dword (Proxy :: Proxy Z)) mem lower
        -- upper
        offset_shamt<-immUInt8 32
        upper<-binary opRShift addr offset_shamt
        mask<-immUInt64 0x00000000ffffffff
        upper<-binary opAnd upper mask
        mset (dword (Proxy :: Proxy N8)) mem upper
        return $ Typedef mem
        
    -- For Typekernel we only use a GDT with 7 elements.
    type TypekernelGDT = GDTTable N7
    data Stx86_64'
    type Stx86_64=Typedef Stx86_64' (Product (Ref TypekernelGDT) (Ref IDTTable))

    gdtElemCnt :: GDTTable s->Proxy s
    gdtElemCnt _=Proxy
    gdtTableSize :: (KnownNat s)=>GDTTable s->Int
    gdtTableSize table = (natToInt (gdtElemCnt table) + 1) * (natToInt $ memSize (Proxy :: Proxy GDTItem))
    -- TODO: virtual address versus real address.
    lgdt :: (KnownNat s)=>GDTTable s->C ()
    lgdt table = do
        id<-newIdent
        emit $ "struct {uint16_t pd_lim; uint64_t pd_base;} __attribute__ ((packed)) "++id++ ";"
        limit<-immUInt16 $ fromIntegral ((gdtTableSize table) -1)
        let base=memStart $ newtypeMem table 
        emit $ id++".pd_lim="++(metadata limit)++";"
        emit $ id++".pd_base="++(metadata base)++";"
        emit $ "Print(L\"limit = %ld, base = %lx\\n\", (uint64_t)"++id++".pd_lim, "++id++".pd_base);"
        emit $ "asm volatile (\"lgdt (%0);\" :: \"r\"(&"++id++"):\"memory\");"
    lidt :: IDTTable->C ()
    lidt table = do
        id<-newIdent
        emit $ "struct {uint16_t pd_lim; uint64_t pd_base;} __attribute__ ((packed)) "++id++ ";"
        limit<-immUInt16 $ (256*16-1)
        let base=memStart $ newtypeMem table 
        emit $ id++".pd_lim="++(metadata limit)++";"
        emit $ id++".pd_base="++(metadata base)++";"
        emit "asm volatile(\"cli;\");"
        emit $ "asm volatile (\"lidt (%0);\" :: \"r\"(&"++id++"):\"memory\");"

    --lidt :: IDTTable->C ()
    -- Initialize an x86_64 CPU core, with GDT and IDT ready.
    initializeX86_64 :: (MonadC m)=>Memory (SizeOf TypekernelGDT)->Memory (SizeOf IDTTable)->Constructor m IDTTable->Constructor m Stx86_64
    initializeX86_64 mgdt midt ctoridt mem= do
        liftC $ emit "asm volatile(\"cli;\");"
        liftC $ emit $ "Print(L\"Start init\\n\");"
        zero<-liftC $ immUInt64 0
        gdt<-tssToGDT zero mgdt
        liftC $ emit $ "Print(L\"lgdt\\n\");"
        --liftC $ lgdt gdt
        liftC $ emit $ "Print(L\"lgdt done\\n\");"
        liftC $ emit $ "Print(L\"lgdt done\\n\");"
        liftC $ emit $ "Print(L\"lgdt done\\n\");"
        liftC $ emit $ "Print(L\"lgdt done\\n\");"
        -- Load TSS.
        --emit $ "asm volatile (\"ltr $"++(show 7*8)++";\");"
        liftC $ emit $ "Print(L\"Start lidt\\n\");"
        liftC $ emit $ "Print(L\"Start lidt\\n\");"
        liftC $ emit $ "Print(L\"Start lidt\\n\");"
        idt<-ctoridt midt
        liftC $ emit $ "Print(L\"lidt\\n\");"
        liftC $ lidt idt
        --liftC $ emit $ "for(;;);"
        --liftC $ emit $ "Print(L\"lidt done\\n\");"
        --liftC $ emit $ "Print(L\"lidt done\\n\");"
        --liftC $ emit $ "Print(L\"lidt done\\n\");"
        --liftC $ emit $ "Print(L\"lidt done\\n\");"
        --liftC $ emit $ "Print(L\"lidt done\\n\");"
        ctorNewtype (ctorProd (refAddress (Proxy :: Proxy TypekernelGDT) mgdt) (refAddress (Proxy :: Proxy IDTTable)  midt)) mem

    