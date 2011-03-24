{-# LANGUAGE TupleSections, DoRec #-}

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

module System.Hiernotify (Difference (..), Configuration (..), Controller (..) , ControllerConfiguration (..), Ermes (..), mkErmes) where

import Control.Applicative ((<$>))
import Data.Monoid (Monoid (..), mempty, mappend)
import Control.Monad (guard, forever,when , void)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM -- (  newTVar, readTVar, writeTVar, atomically)
import Control.Arrow (first)
import Data.List ((\\), nub, intersect)
import qualified System.Timer.Updatable as T
-- | Difference datatype containing a difference as three sets of paths
data Difference = Difference {
  created :: [FilePath], -- ^ Files appeared
  deleted :: [FilePath], -- ^ Files disappeared
  modified :: [FilePath] -- ^ Files modified
  } deriving (Show, Eq)


-- half correct instance. It forces files which have been deleted and created to be marked as modifications. It's not correct as a delete after a create is not a modification. But correcting this bug involves mostly comparing timestamps correctly, because it can happen inside one element of the mappend.
instance Monoid Difference where
  Difference n d m `mappend` Difference n' d' m' = let
    mm = nub $ m ++ m'
    nn = nub $ n ++ n'
    dd = nub $ d ++ d' 
    in Difference ((nn \\ dd) \\ mm) ((dd \\ nn) \\ mm) (nub $ mm ++ intersect nn dd)
  mempty = Difference [] [] []

  
update :: [FilePath] -> Difference -> [FilePath]
update ps (Difference ns ds ms) = nub $ (ps ++ ns ++ ms) \\ ds

-- | configuration to make an Ermes
data Configuration = Configuration 
  { top :: FilePath
  , silence :: Int
  , select :: FilePath -> Bool
  }

data ControllerConfiguration = ControllerConfiguration
  { _top :: FilePath
  , _select :: FilePath -> Bool
  , _diffs :: Difference -> IO ()
  }



-- | An abstract Controller. Parametrized on its configuration, it runs in its thread 
newtype Controller = Controller (ControllerConfiguration -> IO (IO (), [FilePath]))

-- | an Ermes is an object controlling a hierarchy. Its difference method will block until a Difference is available and at least a time of peace has elapsed.Reading an Ermes calling difference will result in deleting the difference and updating the list of paths. The list of path along the difference is always the list of path which the difference refers to.
data Ermes = Ermes 
  { difference :: IO (Difference,[FilePath])
  , kill :: IO () 
  }

-- | make an Ermes given a Controller and the Ermes configuration
mkErmes :: Controller -> Configuration -> IO Ermes
mkErmes (Controller controller) co = do
  timer <- newTVarIO Nothing -- timer
  let comunicate ermes = atomically $ do 
        readTVar timer >>= maybe (return ()) (void . T.wait) 
        (d,p) <- readTVar ermes
        when (d == mempty) retry
        writeTVar ermes (mempty,update p d)
        return (d,p)
  let contribute ermes d = do
        mt <- atomically $ do 
          readTVar ermes >>= writeTVar ermes . first (`mappend` d)
          readTVar timer 
        case mt of
          Nothing -> T.parallel (atomically $ writeTVar timer Nothing) (silence co) >>= atomically . writeTVar timer . Just 
          Just t -> T.renew t $ silence co
  rec   (t,ps) <- controller $ ControllerConfiguration (top co) (select co) (contribute ermes)
        ermes <- newTVarIO (mempty,ps)
  let kill' = t >> atomically (readTVar timer) >>= maybe (return ()) T.kill
  return $ Ermes (comunicate ermes) kill' 

    



