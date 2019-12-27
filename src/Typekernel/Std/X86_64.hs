{-# LANGUAGE DataKinds, PolyKinds, NumericUnderscores, ScopedTypeVariables, FlexibleContexts, TypeFamilies, TypeOperators #-}
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


    import Typekernel.Vec

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
    type GDTTable (s::Nat) = Typedef (GDTTable' s) (StaticArray (NAdd s 1) GDTItem)

    -- Given tss value, yield ctor.
    -- Well the signature become unreadable...
    tssToGDT::(MonadC m)=>UInt64->Constructor m (GDTTable N7)
    tssToGDT tss mem=do
        gdtimm<-mapMV (liftC . immUInt64 . fromIntegral) (0-:kcode-:ucode-:kdata-:udata-:ucode32-:udata32-:nilV)
        let initializer=(mapV (\(ui::UInt64)->liftC . (ctorNewtype $ ctorBasic ui)) (gdtimm ++: (tss -: nilV))) 
        ctorNewtype (ctorArray initializer) mem

    data IDTItem'
    type IDTItem = Typedef IDTItem' (Product (Basic UInt64) (Basic UInt64))
    data IDTTable'
    type IDTTable = Typedef IDTTable' (StaticArray N256 IDTItem)

    emptyIDTItem :: (MonadC m)=>Constructor m IDTItem
    emptyIDTItem mem = liftC $ ctorNewtype (ctorProd zeroBasic zeroBasic) mem

    data Ring = Ring0 | Ring1 | Ring2 | Ring3
    ringToInt :: Ring->Int
    ringToInt Ring0=0
    ringToInt Ring1=1
    ringToInt Ring2=2
    ringToInt Ring3=3

    getVectorEntry :: USize->C USize
    getVectorEntry id = do
        onceC "x86_64_get_vector_entry" $ emitCDecl ["uint64_t x86_64_get_vector_entry(uint64_t id){extern uint64_t __vectors[]; return __vectors[id];}"]
        f<-externFunction "x86_64_get_vector_entry" (Proxy :: Proxy (UInt64, Void)) (Proxy :: Proxy UInt64)
        invoke f (id, Void)

    
    vectoredIDTItem :: (MonadC m)=>Int->Bool->Ring->Constructor m IDTItem
    vectoredIDTItem index isintr ring mem = liftC $ do
        index_imm<-immUInt64 $ fromIntegral index
        addr<-getVectorEntry index_imm
        --emit $ "EFI_LOADED_IMAGE *loaded_image = NULL;"
        --emit $ "uefi_call_wrapper(SystemTable->BootServices->HandleProtocol,3, ImageHandle,&LoadedImageProtocol,(void **)&loaded_image);"
        --emit $ "extern uint64_t vector"++(show index)++";"
        --emit $ (metadata addr) ++" = ((uint64_t)(&vector"++(show index)++"));"
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
    
    externlgdtidt :: C ()
    externlgdtidt = onceC "externlgdtidt"$ do
        let emit s=emitCDecl [s]
        emit "uint64_t fn_lgdt(uint16_t limit, uint64_t base){"
        emit "    struct {uint16_t pd_lim; uint64_t pd_base;} __attribute__ ((packed)) item;"
        emit "    item.pd_lim=limit; item.pd_base=base;"
        emit "    asm volatile (\"lgdt (%0);\" :: \"r\"(&item):\"memory\");return 0;"
        emit "}"
        emit "uint64_t fn_lidt(uint16_t limit, uint64_t base){"
        emit "    struct {uint16_t pd_lim; uint64_t pd_base;} __attribute__ ((packed)) item;"
        emit "    item.pd_lim=limit; item.pd_base=base;"
        emit "    asm volatile(\"cli;\");"
        emit "    asm volatile (\"lidt (%0);\" :: \"r\"(&item):\"memory\");return 0;"
        emit "}"
        return ()
    -- TODO: virtual address versus real address.
    lgdt :: (KnownNat s)=>GDTTable s->C ()
    lgdt table = do
        externlgdtidt
        f<-externFunction "fn_lgdt" (Proxy :: Proxy (UInt16, (UInt64, Void))) (Proxy :: Proxy UInt64)
        limit<-immUInt16 $ fromIntegral ((gdtTableSize table) -1)
        let base=memStart $ newtypeMem table 
        base<-cast (Proxy :: Proxy UInt64) base
        invoke f (limit, (base, Void))
        return ()
    lidt :: IDTTable->C ()
    lidt table = do
        externlgdtidt
        f<-externFunction "fn_lidt" (Proxy :: Proxy (UInt16, (UInt64, Void))) (Proxy :: Proxy UInt64)
        limit<-immUInt16 $ (256*16-1)
        let base=memStart $ newtypeMem table
        base<-cast (Proxy :: Proxy UInt64) base
        (PhysAddr base)<-virtToPhys (VirtAddr base)
        invoke f (limit, (base, Void))
        return ()

    --lidt :: IDTTable->C ()
    -- Initialize an x86_64 CPU core, with GDT and IDT ready.
    initializeX86_64 :: (MonadC m)=>Memory (SizeOf TypekernelGDT)->Memory (SizeOf IDTTable)->Constructor m IDTTable->Constructor m Stx86_64
    initializeX86_64 mgdt midt ctoridt mem= do
        --liftC $ emit "asm volatile(\"cli;\");"
        --liftC $ emit $ "Print(L\"Start init\\n\");"
        zero<-liftC $ immUInt64 0
        liftC $ cli
        gdt<-tssToGDT zero mgdt
        --liftC $ emit $ "Print(L\"lgdt\\n\");"
        --liftC $ lgdt gdt
        --liftC $ emit $ "Print(L\"lgdt done\\n\");"
        --liftC $ emit $ "Print(L\"lgdt done\\n\");"
        --liftC $ emit $ "Print(L\"lgdt done\\n\");"
        --liftC $ emit $ "Print(L\"lgdt done\\n\");"
        -- Load TSS.
        --emit $ "asm volatile (\"ltr $"++(show 7*8)++";\");"
        --liftC $ emit $ "Print(L\"Start lidt\\n\");"
        --liftC $ emit $ "Print(L\"Start lidt\\n\");"
        --liftC $ emit $ "Print(L\"Start lidt\\n\");"
        idt<-ctoridt midt
        --liftC $ emit $ "Print(L\"lidt\\n\");"
        liftC $ lidt idt
        --liftC $ emit $ "for(;;);"
        --liftC $ emit $ "Print(L\"lidt done\\n\");"
        --liftC $ emit $ "Print(L\"lidt done\\n\");"
        --liftC $ emit $ "Print(L\"lidt done\\n\");"
        --liftC $ emit $ "Print(L\"lidt done\\n\");"
        --liftC $ emit $ "Print(L\"lidt done\\n\");"
        liftC $ sti
        ctorNewtype (ctorProd (refAddress (Proxy :: Proxy TypekernelGDT) mgdt) (refAddress (Proxy :: Proxy IDTTable)  midt)) mem

    halt :: C ()
    halt = do
        onceC "x86_64_halt" $ emitCDecl ["uint64_t x86_64_halt(){asm (\"hlt;\");return 0;}"]
        hltf<-externFunction "x86_64_halt" (Proxy :: Proxy Void) (Proxy :: Proxy UInt64)
        invoke hltf Void
        return ()

    cli :: C ()
    cli = do
        onceC "x86_64_cli" $ emitCDecl ["uint64_t x86_64_cli(){asm (\"cli;\");return 0;}"]
        f<-externFunction "x86_64_cli" (Proxy :: Proxy Void) (Proxy :: Proxy UInt64)
        invoke f Void
        return ()

    sti :: C ()
    sti = do
        onceC "x86_64_sti" $ emitCDecl ["uint64_t x86_64_sti(){asm (\"sti;\");return 0;}"]
        f<-externFunction "x86_64_sti" (Proxy :: Proxy Void) (Proxy :: Proxy UInt64)
        invoke f Void
        return ()

    break :: C ()
    break = do
        onceC "x86_64_break" $ emitCDecl ["uint64_t x86_64_break(){asm (\"int $0x03;\");return 0;}"]
        f<-externFunction "x86_64_break" (Proxy :: Proxy Void) (Proxy :: Proxy UInt64)
        invoke f Void
        return ()
    newtype VirtAddr = VirtAddr {virtToRaw :: UInt64}
    newtype PhysAddr = PhysAddr {physToRaw :: UInt64}
    physicalOffset = 0xFFFF800000000000
    readCR3 :: C UInt64
    readCR3 = do
        onceC "x86_64_read_cr3" $ emitCDecl ["uint64_t x86_64_read_cr3(){uint64_t cr3; __asm__ __volatile__ (\"mov %%cr3, %%rax\\n\\t\"\"mov %%eax, %0\\n\\t\": \"=m\" (cr3): /* no input */: \"%rax\");return cr3;}"]
        fn<-externFunction "x86_64_read_cr3" (Proxy :: Proxy Void) (Proxy :: Proxy UInt64)
        addr<-invoke fn Void
        return addr
    
    physToVirt :: PhysAddr->C VirtAddr
    physToVirt x = do
        off<-immUInt64 physicalOffset
        val<-binary opAdd off (physToRaw x)
        return $ VirtAddr val

    physToPtr :: (FirstClass a)=>PhysAddr->C (Ptr a)
    physToPtr x = do
        (VirtAddr addr)<-physToVirt x
        cast (Proxy::Proxy (Ptr a)) addr

    stepDownPageTable :: PhysAddr->UInt64->C PhysAddr
    stepDownPageTable base offset=do
        virt<-physToVirt base
        three<-immUInt8 3
        offset<-binary opLShift offset three
        addr<-binary opAdd (virtToRaw virt) offset
        ptr<-cast (Proxy :: Proxy (Ptr UInt64)) addr
        ptval<-deref ptr
        mask<-immUInt64 0x000f_ffff_ffff_f000
        ptval<-cast (Proxy :: Proxy UInt64) ptval
        ptval<-binary opAnd ptval mask
        return $ PhysAddr ptval

    data VAddrSegment = P1 | P2 | P3 | P4 | VOffset
    virtSegment :: VAddrSegment->VirtAddr->C UInt64
    virtSegment seg vaddr= do
        let addr=virtToRaw vaddr
        let (shamt, mask)=case seg of
                            P4 -> (39, 0x1ff)
                            P3 -> (30, 0x1ff)
                            P2 -> (21, 0x1ff)
                            P1 -> (12, 0x1ff)
                            VOffset -> (0, 0xfff)
        shamt<-immUInt8 shamt
        mask<-immUInt64 mask
        addr<-binary opRShift addr shamt
        binary opAnd addr mask
                    
    virtToPhys :: VirtAddr->C PhysAddr
    virtToPhys x = do
        cr3<-readCR3
        mask<-immUInt64 0xffff_ffff_ffff_f000
        pgtable<-binary opAnd cr3 mask
        p4<-virtSegment P4 x
        p3<-virtSegment P3 x
        p2<-virtSegment P2 x
        p1<-virtSegment P1 x
        voffset<-virtSegment VOffset x
        p<-stepDownPageTable (PhysAddr pgtable) p4
        p<-stepDownPageTable p p3
        p<-stepDownPageTable p p2
        p<-stepDownPageTable p p1
        --pgsz<-immUInt8 12
        --pgo<-binary opLShift p1 pgsz
        --voffset<-binary opAdd pgo voffset
        --p<-stepDownPageTable p p1
        p<-binary opAdd (physToRaw p) voffset
        return $ PhysAddr p
