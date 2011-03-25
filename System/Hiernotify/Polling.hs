
{-# LANGUAGE TupleSections #-}

-- | This module offers a daemon polling a filesystem hierarchy to notify changes.
--
-- This example runs a /n/ seconds console reporter for the activity in hierarchy /p/. Polling delay is 3 seconds. 
-- And it waits a 10 seconds lapse without modifications before reporting. Activities in hierarchy p are reported, while running"
--
-- 
-- @
-- import System.Hiernotify.Polling
-- import Control.Concurrent
--
-- testReport   :: Int -- ^ life span for the program
--    -> FilePath -- ^ hierarchy top
--    -> IO () -- ^ block for life span
-- testReport n p = do 
--  m <- mkPollNotifier 3 (Configuration p 10 (not . isPrefixOf \".\"))
--  p <- forkIO $ forever $ do 
--    x <- difference m 
--    print x 
--  threadDelay $ n * 1000000 -- wait n seconds 
--  stop m
--  killThread p
-- @
--
--

module System.Hiernotify.Polling 
  ( mkPollNotifier
  , Configuration (..)
  , Difference (..) 
  , Notifier (..)
  ) where

import Control.Applicative ((<$>))
import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Control.Concurrent.STM (newTVar, readTVar, writeTVar, atomically)
import Data.Maybe (catMaybes)
import Control.Concurrent.STM.TMonoid (TMonoid, newTMonoid, readTMonoid, writeTMonoid)
import System.Hiernotify.Controller (Controller (..), mkNotifier, NextDiff (..), getRecursiveContents)
import System.Hiernotify (Configuration (..), Notifier (..), Difference (..))
import Data.List ((\\))
import Control.Concurrent.Killable (kill)


-- create a stateful sampling action for a hierarchy. State is needed because we compute diffs 
checkDifference :: (FilePath -> Bool) 
    -> FilePath       -- ^ top directory
    -> IO (IO Difference) -- ^ null initialized stateful action
checkDifference g top' = do  
  t <- atomically $ newTVar [] 
  return $ do 
    xs <- getRecursiveContents g top' 
    ws <- atomically $ do 
      ws <- readTVar t
      writeTVar t xs
      return ws
    let   
      news' = map fst xs \\ map fst ws 
      deleteds' = map fst ws \\ map fst xs
      modified' = catMaybes $ do 
        (x,y) <- xs
        return $  lookup x ws >>= \y' -> if y /= y' then Just x else Nothing
    return $ Difference news' deleteds' modified'


-- track file changes in a hierarchy. This program updates the passed TMonoid. The result action kills the poller daemon 
trackPollFiles  :: Int      -- ^ polling delay in seconds
    -> (FilePath -> Bool)  -- ^ path filter
    -> FilePath     -- ^ hierarchy top
    -> TMonoid Difference   -- ^ a monoidal STM memory cell storing last modifications
    -> IO ThreadId   -- ^ the action to kill the tracking program
trackPollFiles n g top' tm = do
  s <- checkDifference g top' 
  k <- forkIO . forever $ threadDelay (1000000 * n) >> s >>= atomically . writeTMonoid tm 
  return  k

-- | make a polling notifier , given an interval in seconds
mkPollNotifier  :: Int            -- ^ minimum lapse between polling actions
                -> Configuration  -- ^ notifier configuration 
                -> IO Notifier    -- ^ the polling notifier
mkPollNotifier n (Configuration t s g)  = do 
  tm <- atomically newTMonoid  
  p <- trackPollFiles n g t tm
  let f = NextDiff $ (, f) `fmap` atomically (readTMonoid tm)
  ps0 <- map fst <$> getRecursiveContents g t
  Notifier no k  <- mkNotifier s $ Controller ps0 f
  return $ Notifier no $ kill p >> k



