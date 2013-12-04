{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
import Prelude hiding (log)
import Control.Eff
import Control.Eff.Lift
import Data.Typeable (Typeable)

data Log v = Log String v deriving (Functor, Typeable)

log :: Member Log r => String -> Eff r ()
log txt = send $ \next -> inj (Log txt (next ()))

verboseAddition :: Member Log r => Eff r Int
verboseAddition = do
  log "I'm starting with 1..."
  x <- return 1

  log "and I'm adding 2..."
  y <- return 2

  let r = x + y

  log $ "Looks like the result is " ++ show r
  return r

runLogger :: Eff (Log :> r) a -> Eff r (a, [String])
runLogger logAction = go (admin logAction)
 where
  go (Val v) = return (v, [])
  go (E request) =
    let prefixLogWith txt (v, l) = (v, txt:l)
        performLog (Log txt next) =
          fmap (prefixLogWith txt) (go next)
    in handleRelay request go performLog

runIOLogger :: SetMember Lift (Lift IO) r => Eff (Log :> r) a -> Eff r a
runIOLogger logAction = go (admin logAction)
 where
  go (Val v) = return v
  go (E request) =
    let performLog (Log txt next) =
          lift (putStrLn txt) >> go next
    in handleRelay request go performLog
