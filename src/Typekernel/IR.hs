{-# LANGUAGE DeriveGeneric, FunctionalDependencies, FlexibleInstances #-}
module Typekernel.IR where
    import GHC.Generics
    import Data.Aeson
    import Typekernel.C4mAST
    import Typekernel.Nat
    data IRDatatype=IInt8 | IUInt8 | IInt16 | IUInt16 | IInt32 | IUInt32 | IInt64 | IUInt64 | IMemory Int |
                    IPInt8 | IPUInt8 | IPInt16 | IPUInt16 | IPInt32 | IPUInt32 | IPInt64 | IPUInt64 deriving (Generic, Show)
    data IRLiteral = IInteger Integer | IFloat Float | IDouble Double deriving (Generic, Show)
    data IRStmt =
        IRFun {
            iName :: String,
            iReturnField :: [(IRDatatype, String)],
            iArgumentField :: [(IRDatatype, String)],
            iFuncBody :: IRStmt
        } |
        IRImm {
            iUName :: (IRDatatype, String),
            iLiteral :: IRLiteral
        } |
        IRBinary {
            iUName :: (IRDatatype, String),
            iOperator :: String,
            iOperand1 :: (IRDatatype, String),
            iOperand2 :: (IRDatatype, String)
        } | 
        IRUnary {
            iUName :: (IRDatatype, String),
            iOperator :: String,
            iOperand :: (IRDatatype, String)
        } |
        IRInvoke {
            iLName :: [(IRDatatype, String)],
            iArguments :: [(IRDatatype, String)]
        } |
        IRTernary {
            iLName :: [(IRDatatype, String)],
            iTOperand1 :: [(IRDatatype, String)],
            iTOperand2 :: [(IRDatatype, String)],
            iTOperand3 :: [(IRDatatype, String)]
        } |
        IRSeq {
            iSequence :: [IRStmt]
        }
        deriving (Generic, Show)

    instance ToJSON IRDatatype
    instance FromJSON IRDatatype
    instance ToJSON IRLiteral
    instance FromJSON IRLiteral
    instance ToJSON IRStmt
    instance FromJSON IRStmt

    class IRDatatypeCast a b | a->b where
        castDatatype :: a->(b, String)

    instance IRDatatypeCast Int8 IRDatatype where
        castDatatype (Int8 x)=(IInt8, x)
    instance IRDatatypeCast UInt8 IRDatatype where
        castDatatype (UInt8 x)=(IUInt8, x)
    instance IRDatatypeCast Int16 IRDatatype where
        castDatatype (Int16 x)=(IInt16, x)
    instance IRDatatypeCast UInt16 IRDatatype where
        castDatatype (UInt16 x)=(IUInt16, x)
    instance IRDatatypeCast Int32 IRDatatype where
        castDatatype (Int32 x)=(IInt32, x)
    instance IRDatatypeCast UInt32 IRDatatype where
        castDatatype (UInt32 x)=(IUInt32, x)
    instance IRDatatypeCast Int64 IRDatatype where
        castDatatype (Int64 x)=(IInt64, x)
    instance IRDatatypeCast UInt64 IRDatatype where
        castDatatype (UInt64 x)=(IUInt64, x)
    instance (KnownNat n)=>IRDatatypeCast (Memory n) IRDatatype where
        castDatatype memory@(Memory (Ptr mem))=(IMemory (natToInt $ extractNat memory), mem)
    
    instance IRDatatypeCast (Ptr Int8) IRDatatype where
        castDatatype (Ptr x)=(IPInt8, x)
    instance IRDatatypeCast (Ptr UInt8) IRDatatype where
        castDatatype (Ptr x)=(IPUInt8, x)
    instance IRDatatypeCast (Ptr Int16) IRDatatype where
        castDatatype (Ptr x)=(IPInt16, x)
    instance IRDatatypeCast (Ptr UInt16) IRDatatype where
        castDatatype (Ptr x)=(IPUInt16, x)
    instance IRDatatypeCast (Ptr Int32) IRDatatype where
        castDatatype (Ptr x)=(IPInt32, x)
    instance IRDatatypeCast (Ptr UInt32) IRDatatype where
        castDatatype (Ptr x)=(IPUInt32, x)
    instance IRDatatypeCast (Ptr Int64) IRDatatype where
        castDatatype (Ptr x)=(IPInt64, x)
    instance IRDatatypeCast (Ptr UInt64) IRDatatype where
        castDatatype (Ptr x)=(IPUInt64, x)
    instance IRDatatypeCast (Ptr (Ptr a)) IRDatatype where
        castDatatype (Ptr x)=(IPUInt64, x)