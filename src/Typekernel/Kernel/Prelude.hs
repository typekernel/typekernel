{-# LANGUAGE FlexibleContexts #-}
-- Shortcut for writing code.
module Typekernel.Kernel.Prelude where
    import qualified Prelude
    import qualified Control.Monad as M
    import Prelude (($), (.), pure, Bool(True, False))
    import Control.Monad
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    import Typekernel.Debug
    myliftM2 :: (Monad m)=>(a->b->m c)->m a->m b->m c
    myliftM2=((join.).). liftM2
    (+) :: (C4mAST m, Binary Add t1 t2 a) => m t1->m t2->m a
    (-) :: (C4mAST m, Binary Sub t1 t2 a) => m t1->m t2->m a
    (*) :: (C4mAST m, Binary Mul t1 t2 a) => m t1->m t2->m a
    (/) :: (C4mAST m, Binary Div t1 t2 a) => m t1->m t2->m a

    (+)=myliftM2 (\a b->  (binary opAdd a b))
    (-)=myliftM2 (\a b->  (binary opSub a b))
    (*)=myliftM2 (\a b->  (binary opMul a b))
    (/)=myliftM2 (\a b->  (binary opDiv a b))

    infixl 6 +
    infixl 6 -
    infixl 7 *
    infixl 7 /

    i=immInt64