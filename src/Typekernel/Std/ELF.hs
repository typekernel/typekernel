{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Typekernel.Std.ELF where
    import Typekernel.Memory
    import Typekernel.Nat
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    import Typekernel.MLens
    import Typekernel.Std.Vec
    import Data.Proxy
    import Control.Monad.Trans.Reader
    import Control.Monad
    import Control.Monad.Trans.Class
    import Data.Conduit
    import Typekernel.Std.Iteratee
    import Typekernel.Bound
    newtype ELFHeader = ELFHeader {elfHeaderMem :: Memory N64}
    newtype ProgramHeader = ProgramHeader {phOffset :: UInt64}

    newtype ELFParser m a = ELFParser {elfToBufferParser :: BufferParser m a} deriving (Monad, Functor, Applicative, MonadTrans)
    instance (MonadC m)=>MonadC (ELFParser m) where
        liftC m=lift $ liftC m

    runELFParser :: Buffer->ELFParser env a->env a
    runELFParser buf (ELFParser p)=runBufferParser buf p
    magic :: (MonadC env)=>ELFParser env UInt32
    magic = ELFParser $ (lift $ liftC $ immUInt64 0) >>= readWord
        
    programEntry :: (MonadC env)=>ELFParser env UInt64
    programEntry = ELFParser $ (lift $ liftC $ immUInt64 24) >>= readDword

    programHeaderOffset :: (MonadC env)=>ELFParser env UInt64
    programHeaderOffset = ELFParser $ (lift $ liftC $ immUInt64 32) >>= readDword
    programHeaderSize :: (MonadC env)=>ELFParser env UInt16
    programHeaderSize = ELFParser $ (lift $ liftC $ immUInt64 54) >>= readHalf
    programHeaderCount :: (MonadC env)=>ELFParser env UInt16
    programHeaderCount = ELFParser $ (lift $ liftC $ immUInt64 56) >>= readHalf
    programHeaders :: (MonadC env)=>ConduitT () ProgramHeader (ELFParser env) ()
    programHeaders = do
        count<-lift $ programHeaderCount
        --sz<-
        iterator<-recursion $ \f (index::UInt16, (_::Typekernel.C4mAST.Void))->do
            valid<-liftC $ binary opCLE index count
            ifS valid (do 

                --lift $ yield $ ProgramHeader v
                --liftC $ immUInt64 0
                -- Iterate next.
                one<-liftC $ immUInt16 1
                newindex<-liftC $ binary opAdd index one
                lift $ lift $ f (newindex, Void)
                return Void
                ) (return Void)
            liftC $ immUInt64 0
            
        zero<-liftC $ immUInt16 0
        invokeS iterator (zero, Void)
        return ()
        


        
    --magic :: (MonadC env)=>MLens env ELFHeader UInt32
    --magic = wrapPred elfHeaderMem (word (Proxy :: Proxy N0))

    --programEntry :: (MonadC env)=>MLens env ELFHeader UInt64
    --programEntry = wrapPred elfHeaderMem (dword (Proxy :: Proxy N24))

    --programHeaders :: (MonadC env)=>ELFHeader->BufferParser env ProgramHeader
    --programHeaders = liftC . (fmap ProgramHeader) . (remoteMemory (Proxy :: Proxy N32)) . elfHeaderMem


    