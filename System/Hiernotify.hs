{-# LANGUAGE TupleSections #-}

-- | This module offers a daemon polling a filesystem hierarchy to notify changes to a given IO action.
--
-- This example runs a /n/ seconds console reporter for the activity in hierarchy /p/. Polling delay is 3 seconds. 
-- And it waits 2 silent polling samples before reporting to console.
--
-- Medium responsiveness for an isolated change is then 3 * (2 + 1/2) seconds
-- 
-- @
-- testReport   :: Int -- ^ life span for the program
--    -> FilePath -- ^ hierarchy top
--    -> IO () -- ^ block for life span
-- testReport n p = do 
--  k <- onDifferenceDaemon  3 2 (not . isPrefixOf \".\") p  (print . report) -- boot the onDifferenceDaemon
--  threadDelay $ n * 1000000 -- wait n seconds
--  k -- kill the onDifferenceDaemon
--  where report (Difference nn dd mm) = map length [nn,dd,mm]
-- @
--

module System.Hiernotify (Difference (..), onDifferenceDaemon) where

import Control.Applicative ((<$>))
import Data.Monoid (Monoid (..), mempty, mappend)
import Control.Monad.List (ListT (ListT), runListT,guard, forever)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.STM (  newTVar, readTVar, writeTVar, atomically)
import Control.Monad.Trans (lift)
import Data.List ((\\), nub, intersect)
import Data.Maybe (catMaybes)
import System.Directory (getModificationTime, doesDirectoryExist, getDirectoryContents)
import System.FilePath (normalise, (</>))
import System.Time (ClockTime)
import Control.Concurrent.STM.TMonoid (TMonoid, newDelayedTMonoid, readTMonoid, writeTMonoid)

-- | Difference datatype containing a difference as three sets of paths
data Difference = Difference {
  created :: [FilePath], -- ^ Files appeared
  deleted :: [FilePath], -- ^ Files disappeared
  modified :: [FilePath] -- ^ Files modified
  } deriving (Show, Eq)


-- Get all paths under a directory
getRecursiveContents 
  :: (FilePath -> Bool) -- ^ guard  
  -> FilePath       -- ^ top
  -> IO [(FilePath, ClockTime)]  -- ^ List of files found
getRecursiveContents g = runListT . getRecursiveContents' where   
  getRecursiveContents' path = do
    pathIsDir <- lift $ doesDirectoryExist path
    if pathIsDir then do 
      name <- ListT $ getDirectoryContents path
      guard . g $ name
      getRecursiveContents' . normalise $ path </> name
      else (path,) <$> lift (getModificationTime path)


-- half correct instance. It forces files which have been deleted and created to be marked as modifications. It's not correct as a delete after a create is not a modification. But correcting this bug involves mostly comparing timestamps correctly, because it can happen inside one element of the mappend.
instance Monoid Difference where
  Difference n d m `mappend` Difference n' d' m' = let
    mm = nub $ m ++ m'
    nn = nub $ n ++ n'
    dd = nub $ d ++ d' 
    in Difference ((nn \\ dd) \\ mm) ((dd \\ nn) \\ mm) (nub $ mm ++ intersect nn dd)
  mempty = Difference [] [] []

-- create a stateful sampling action for a hierarchy. State is needed because we compute diffs 
checkDifference :: (FilePath -> Bool) 
    -> FilePath       -- ^ top directory
    -> IO (IO Difference) -- ^ null initialized stateful action
checkDifference g top = do  
  t <- atomically $ newTVar [] 
  return $ do 
    xs <- getRecursiveContents g top 
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
    -> IO (IO ())   -- ^ the action to kill the tracking program
trackPollFiles n g top tm = do
  s <- checkDifference g top 
  k <- forkIO . forever $ threadDelay (1000000 * n) >> s >>= atomically . writeTMonoid tm 
  return $ killThread k

-- | Execute an action on file changes in a hierarchy. 
onDifferenceDaemon  :: Int    -- ^ polling delay in seconds
      -> Int    -- ^ number of no-change delays before running the action 
      -> (FilePath -> Bool) -- ^ path filter
      -> FilePath -- ^ file hierarchy top
      -> (Difference -> IO ()) -- ^ the action executed on a modification
      -> IO (IO ()) -- ^ the action to kill the daemon
onDifferenceDaemon n n2 g top f = do 
  tm <- atomically $ newDelayedTMonoid n2 
  kt' <- trackPollFiles n g top tm
  k <- forkIO . forever $ atomically (readTMonoid tm) >>= f 
  return $ killThread k >> kt'



