module Control.Concurrent.STM.TMonoid (TMonoid, writeTMonoid, readTMonoid, newTMonoid) where

import Control.Concurrent.STM
import Data.Monoid 
import Control.Monad (when)

-- | a concurrent STM Monoid
data TMonoid m = TMonoid {
  writeTMonoid :: m -> STM (), -- ^ mappend the value
  readTMonoid :: STM m -- ^ peek the monoid and reset it
  }

-- | create a TMonoid for a comparable Monoid. The created TMonoid waits for an empty update to release a read
newTMonoid :: (Monoid m, Eq m) 
  => STM (TMonoid m)  -- ^ a delayed TMonoid
newTMonoid = do
  x <- newTVar mempty -- the monoid
  let   
    write y   = readTVar x >>= writeTVar x . (`mappend` y)  --update monoid and reset counter
    read' = do
      y <- readTVar x
      when (y == mempty) retry -- on empty monoid and lately busy 
      writeTVar x mempty -- reset the monoid
      return y
  return $ TMonoid write read'

