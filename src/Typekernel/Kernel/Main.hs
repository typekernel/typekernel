{-# LANGUAGE QuasiQuotes, DataKinds, ScopedTypeVariables, GeneralisedNewtypeDeriving #-}
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
    import Typekernel.Std.Log
    import Control.Monad.Trans.Reader
    import Control.Monad.Fix
    import Control.Monad.Trans
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
    #include <stdint.h>
    typedef uint64_t usize_t;
    typedef int64_t size_t;
        usize_t kentry();
        uint8_t __attribute__ ((aligned (4096))) gdt_page[4096];
        uint8_t __attribute__ ((aligned (4096))) idt_page[4096];
        void _kentry(){
            kentry();
        }
        // Serial
        #define PORT 0x3f8   /* COM1 */

        static inline void outb(uint16_t port, uint8_t val)
        {
            asm volatile ( "outb %0, %1" : : "a"(val), "Nd"(port) );
            /* There's an outb %al, $imm8  encoding, for compile-time constant port numbers that fit in 8b.  (N constraint).
            * Wider immediate constants would be truncated at assemble-time (e.g. "i" constraint).
            * The  outb  %al, %dx  encoding is the only option for all other cases.
            * %1 expands to %dx because  port  is a uint16_t.  %w1 could be used if we had the port number a wider C type */
        }
        static inline uint8_t inb(uint16_t port)
        {
            uint8_t ret;
            asm volatile ( "inb %1, %0"
                        : "=a"(ret)
                        : "Nd"(port) );
            return ret;
        }
        void init_serial() {
            outb(PORT + 1, 0x00);    // Disable all interrupts
            outb(PORT + 3, 0x80);    // Enable DLAB (set baud rate divisor)
            outb(PORT + 0, 0x03);    // Set divisor to 3 (lo byte) 38400 baud
            outb(PORT + 1, 0x00);    //                  (hi byte)
            outb(PORT + 3, 0x03);    // 8 bits, no parity, one stop bit
            outb(PORT + 2, 0xC7);    // Enable FIFO, clear them, with 14-byte threshold
            outb(PORT + 4, 0x0B);    // IRQs enabled, RTS/DSR set
        }

        int serial_received() {
            return inb(PORT + 5) & 1;
        }
        
        char read_serial() {
            while (serial_received() == 0);
            return inb(PORT);
        }
        int is_transmit_empty() {
            return inb(PORT + 5) & 0x20;
        }
        
        uint64_t write_serial(char a) {
            while (is_transmit_empty() == 0);
            outb(PORT,a);
            return 0;
        }

        uint64_t get_gdt_page(){
            return gdt_page;
        }

        uint64_t get_idt_page(){
            return idt_page;
        }

    |]
    serial :: C (Fn Void UInt8, Fn (UInt8, Void) UInt64)
    serial = do
        readC<-externFunction "read_serial" (Proxy :: Proxy Void) (Proxy :: Proxy UInt8) 
        writeC<-externFunction "write_serial" (Proxy :: Proxy (UInt8, Void)) (Proxy :: Proxy UInt64)
        return (readC, writeC)
    debug_gdt_idt_pages :: C (UInt64, UInt64)
    debug_gdt_idt_pages = do
        gdtP<-externFunction "get_gdt_page" (Proxy :: Proxy Void) (Proxy :: Proxy UInt64) 
        idtP<-externFunction "get_idt_page" (Proxy :: Proxy Void) (Proxy :: Proxy UInt64)
        gdtP<-invoke gdtP Void
        idtP<-invoke idtP Void
        return (gdtP, idtP)
    data KernelServices=KernelServices {
        kernelLogger :: (SFn C (Ptr UInt8, Void) UInt64)

    }
    kernel :: K ()->K ()->C ()
    kernel fn ft= do
        onceC "kernel" $ emitCDecl [kernel_hardcode]
        (readSerial, writeSerial)<-serial
        --recursion :: (FirstClass b, FirstClassList a, MonadC m)=>((a->m b)->a->(forall s. RAII s m b))->m (SFn m a b)
        kernel_logger<-recursion (\(f::(Ptr UInt8, Void)->C UInt64) (l :: (Ptr UInt8, Void))->do
            let (pchr, _)=l
            chr<-liftC $ deref pchr
            nul<-liftC $ immUInt8 0
            str_ended<-liftC $ binary opCEQ chr nul
            (val, _)<-ifS str_ended (do
                zero<-liftC $ immUInt64 0
                return (zero, Void)) (do
                liftC $ invoke writeSerial (chr, Void)
                ptr<-liftC $ cast (Proxy :: Proxy UInt64) pchr
                one<-liftC $ immUInt64 1
                ptr<-liftC $ binary opAdd ptr one
                ptr<-liftC $ cast (Proxy :: Proxy (Ptr UInt8)) ptr
                rt<-lift $ lift $ f (ptr, Void)
                return (rt, Void))
            return val)
        --namedFunction :: (FirstClass b, FirstClassList a, MonadC m)=>String->(a->m b)->m (Fn a b)
        kmain<-namedFunction "kmain" (\(_ :: Void)->do
                runReaderT (kernelEnv fn) (KernelServices kernel_logger)
                immUSize 0)
        kentry<-namedFunction "kentry" (\(_ :: Void)->do

            
            invoke kmain Void
            immUSize 0)
        th<-namedFunction "trap_handler" (\(_::Void)->do
            runReaderT (kernelEnv ft) (KernelServices kernel_logger)
            immUSize 0)
        return ()
    --class (Monad m)=>MonadLog m where
    --    logString :: String->m ()
    --    logStringLiteral :: StringLiteral->m ()
    --    logInteger :: UInt64->m ()
    --    logChar :: UInt8->m ()
    --    logHex :: UInt64->m ()
    
    newtype Kernel a = Kernel {kernelEnv :: ReaderT KernelServices C a} deriving (Monad, Functor, Applicative,  MonadFix)
    
    type K=Kernel
    instance (MonadC Kernel) where
        liftC = Kernel . lift
    instance MonadHeap Kernel where
    --    malloc= asks (uefiAllocator 

    
    instance MonadLog Kernel where
        logString x = do
            str<-stringL x
            logStringLiteral str
        logStringLiteral x = do
            logger<-Kernel (asks kernelLogger)
            liftC $ invokeS logger (rawPointer x, Void)
            return ()

            

    kernelEntry :: K ()
    kernelEntry=do
        [logF|Hello, Typekernel!|]
        (gdtp, idtp)<-liftC $ debug_gdt_idt_pages
        ptr<-liftC $ cast (Proxy :: Proxy (Ptr UInt64)) gdtp
        -- Page Table Self-Checking
        magic<-liftC $ immUInt64 0xfeeddeadbeeebeef
        vaddr<-liftC $ cast (Proxy :: Proxy UInt64) ptr
        phys<-liftC $ virtToPhys (VirtAddr vaddr)
        vphys<-liftC $ physToVirt (phys)
        vphys<-liftC $ cast (Proxy :: Proxy (Ptr UInt64)) $ virtToRaw vphys
        liftC $ mref vphys magic
        readback<-liftC $ deref ptr
        cmp<-liftC $ binary opCEQ readback magic
        ifS cmp 
            (do
                lift $ [logF|Correct!|]
                return Void)
            (do
                lift $ [logF|Incorrect!|]
                liftC $ halt
                return Void)
        -- Initialize x86 environment
        gdtp<-liftC $ cast (Proxy :: Proxy (Ptr UInt64)) gdtp
        idtp<-liftC $ cast (Proxy :: Proxy (Ptr UInt64)) idtp
        mem_state<-liftC $ defarr (Proxy :: Proxy (SizeOf Stx86_64))
        let memgdt=Memory gdtp
        let memidt=Memory idtp
        let idt_table=mapV (\x-> (vectoredIDTItem x (x==0x68) Ring0)) $ vectorNat' (Proxy :: Proxy N256)
        let idt_ctor=ctorNewtype (ctorArray idt_table)
        initializeX86_64 memgdt memidt idt_ctor mem_state
        
        [logF|Initialized.|]
        liftC $ foreverLoop $ lift $ liftC $ Typekernel.Std.X86_64.break -- infiloop trigger interrupt
        return ()
    -- Trap Handler
    trapHandler :: K ()
    trapHandler = do
        logString "Interrupt!\\n"
        return ()
    -- Combine two together
    main :: C ()
    main=kernel kernelEntry trapHandler