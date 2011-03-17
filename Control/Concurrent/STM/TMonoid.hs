module Control.Concurrent.STM.TMonoid (TMonoid, writeTMonoid, readTMonoid, newDelayedTMonoid) where

import Control.Concurrent.STM
import Data.Monoid 
import Control.Monad (when)

-- | a concurrent STM Monoid
data TMonoid m = TMonoid {
	writeTMonoid :: m -> STM (), -- ^ mappend the value
	readTMonoid :: STM m -- ^ peek the monoid and reset it
	}

-- | create a TMonoid for a comparable Monoid. The created TMonoid waits for an empty update to release a read
newDelayedTMonoid :: (Monoid m, Eq m) 
	=> Int			-- ^ number of empty mappends before allowing the read 
	-> STM (TMonoid m)	-- ^ a delayed TMonoid
newDelayedTMonoid n = do
	x <- newTVar mempty -- the monoid
	was <- newTVar 0 -- delay counter
	let 	write y	| y == mempty = readTVar was >>= writeTVar was . (+ 1) -- update counter
			| otherwise = readTVar x >>= writeTVar x . (`mappend` y) >> writeTVar was 0 --update monoid and reset counter
		read' = do
			y <- readTVar x
			z <- readTVar was
			when (y == mempty || z < n) retry -- on empty monoid and lately busy 
			writeTVar x mempty -- reset the monoid
			return y
	return $ TMonoid write read'

