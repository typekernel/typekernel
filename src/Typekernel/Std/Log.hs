{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, TemplateHaskell #-}
module Typekernel.Std.Log where
    import Typekernel.C4mAST
    import Typekernel.Std.StringLiteral
    import Data.Proxy
    import Language.Haskell.TH
    import Language.Haskell.TH.Quote
    import Data.List
    class Loggable a where
        putLog :: (MonadLog m)=>a->m ()

    instance Loggable () where
        putLog _=return ()
    --instance (Loggable a, Loggable b)=>Loggable (a, b) where
    --    putLog (a, b)=putLog a >> putLog b
    class (Monad m)=>MonadLog m where
        logString :: String->m ()
        logStringLiteral :: StringLiteral->m ()

    instance Loggable StringLiteral where
        putLog=logStringLiteral

    instance Loggable String where putLog=logString
    {-
    class LogF a b | a->b where
        logf :: Proxy a->b

    secondP :: Proxy (a,b)=>Proxy b
    secondP _ = Proxy
    instance (Loggable a, MonadLog m)=>LogF a (a->m()) where
        logf _=putLog
    instance (Loggable a, LogF b c) => LogF (a, b) (a->c) where
        logf proxy=do

    -}

    instance MonadLog IO where
        logString = putStr
        logStringLiteral (StringLiteral (Ptr id))=putStr $ "{-# StringLiteral <"++id++"> #-}"
    data FToken = FRawString String | FPlaceHolder String (Maybe Name) deriving Show
    data ParserSTM=ParserSTM {isInBracket :: Bool, currentStr :: String, parsedTokens :: [FToken], slashed :: Bool} deriving Show
    

    parseFormat :: String->Either String [FToken]
    parseFormat f=fmap (reverse . filter go . parsedTokens ) $ parseF f (ParserSTM False "" [] False) where
            go (FRawString "")=False
            go _ =True
    parseF :: String->ParserSTM->Either String ParserSTM

    parseF "" s=if isInBracket s then Left "Bracket not closed" else Right s{currentStr = "", parsedTokens = (FRawString $ reverse $ currentStr s) : (parsedTokens s)}
    parseF (x:xs) s@ParserSTM {slashed=True} = parseF xs $ s{currentStr = x : (currentStr s), slashed=False}
    parseF ('\\':xs) s = parseF xs $ s{slashed=True}
    parseF ('{':xs) s= if (isInBracket s) then Left "Embedded bracket is not allowed in format" else parseF xs $ s{isInBracket = True, currentStr = "", parsedTokens = (FRawString $ reverse $ currentStr s) : (parsedTokens s)}
    parseF ('}':xs) s = if(isInBracket s) then parseF xs $ s{isInBracket=False, currentStr="", parsedTokens = (FPlaceHolder ( reverse $ currentStr s) Nothing): parsedTokens s} else Left "Bad right bracket"
    parseF (x:xs) s = parseF xs $ s{currentStr = x : (currentStr s)}
    logFExp :: String->Q Exp
    logFExp format = 
        case parseFormat format of
            (Left err) -> fail err
            (Right tokens)-> do
                expPutLog <- [e|putLog|]
                expBind <- [e|(>>)|]
                let nameph (FPlaceHolder conf _)=do {n<-newName "x"; (return $ FPlaceHolder conf (Just n))}
                    nameph x=return x
                named_tokens<-mapM nameph tokens
                let collect (FPlaceHolder conf (Just name)) (p, o)=((VarP name):p, (Just (AppE (expPutLog) (VarE name))):o)
                    collect (FRawString str) (p,o)=(p, (Just (AppE (expPutLog) (LitE (StringL str)))):o)
                let (pats, ops)=foldr collect ([], []) named_tokens
                let (Just realOps)=foldl1 (\x y->Just $ InfixE x (expBind) y) ops
                return $ LamE pats $ realOps 
    logF :: QuasiQuoter
    logF=QuasiQuoter {quoteExp=logFExp}