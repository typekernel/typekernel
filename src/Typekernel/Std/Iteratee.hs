module Typekernel.Std.Iteratee where
    -- Simple iteratee pattern.
    import Data.Conduit
    import Typekernel.Transpiler
    import Control.Monad.Trans.Class
    instance (MonadC m)=>MonadC (ConduitT i o m) where
        liftC = lift . liftC