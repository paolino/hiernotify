{-# LANGUAGE TupleSections #-}
module System.Hiernotify.Controller  where


import Data.Time.Clock (UTCTime)
import Control.Applicative ((<$>))
import Data.List ((\\),nub)
import Data.Monoid (Monoid (..), mempty, mappend)
import Control.Monad (guard, when , void)
import Control.Monad.List (ListT(ListT), runListT, lift)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM -- (  newTVar, readTVar, writeTVar, atomically)
import Control.Arrow (first)
import qualified System.Timer.Updatable as T
import Control.Concurrent.Killable (kill)
import System.Hiernotify (Notifier (..), DifferenceP (..), Difference)
import System.Directory (getModificationTime, doesDirectoryExist, getDirectoryContents)
import System.FilePath (normalise, (</>))
import Data.Int

update :: [FilePath] -> Difference -> [FilePath]
update ps (DifferenceP ns ds ms) = nub $ (ps ++ ns ++ ms) \\ ds


-- | An abstract Controller. Parametrized on its configuration, it runs in its thread 
data Controller = Controller {base :: [FilePath] , step :: NextDiff}

-- | Infinite waiters for differences. This wrap an IO action which should block until a non empty difference is given
newtype NextDiff = NextDiff (IO (Difference,NextDiff)) 

-- | Get all paths under a directory
getRecursiveContents 
  :: (FilePath -> Bool) -- ^ guard  
  -> FilePath       -- ^ top
  -> IO [(FilePath, UTCTime)]  -- ^ List of files found
getRecursiveContents g = runListT . getRecursiveContents' where   
  getRecursiveContents' path = do
    pathIsDir <- lift $ doesDirectoryExist path
    if pathIsDir then do 
      name <- ListT $ getDirectoryContents path
      guard . g $ name
      getRecursiveContents' . normalise $ path </> name
      else (path,) <$> lift (getModificationTime path)


-- | make an Notifier given a Controller and the Notifier configuration
mkNotifier  :: Int64          -- the silence time lapse in seconds to respect before notifying
            -> Controller   -- the implementation for the listener
            -> IO Notifier  -- a fresh, running Notifier using the given Controller
mkNotifier s (Controller ps0 nd0) = do
  ermes <- newTVarIO (mempty,ps0) -- last diff and relative paths
  timer <- newTVarIO Nothing -- timer
  let delta = s * 10 ^ (6 :: Int) -- silence in microseconds
  -- this waits until conditions release a new diff
  let comunicate = atomically $ do 
        readTVar timer >>= maybe (return ()) (void . T.wait) -- retry on active timer
        (d,p) <- readTVar ermes 
        when (d == mempty) retry -- retry on no diff
        writeTVar ermes (mempty,update p d) -- set an empty diff and update the base
        return (d,p) -- the old diff and its base
  -- this wait for changes from the implementation, a never ending loop
  let contribute (NextDiff nd) = do
        (d,nd') <- nd  -- wait until a difference, also receive the new NextDiff
        -- create or update the timer, as an event occurred
        mt <- atomically $ readTVar timer 
        case mt of
          Nothing -> T.parallel (atomically $ writeTVar timer Nothing) delta >>= atomically . writeTVar timer . Just 
          Just t -> T.renewIO t $ delta
        -- update the differende monoid
        atomically $ readTVar ermes >>= writeTVar ermes . first (`mappend` d)
        -- tail recurse with the new NextDiff
        contribute nd'

  -- let the contribute run in its thread
  p <- forkIO $ contribute nd0
  
  -- make the notifier with the blocking comunicate and the cleaning action on the
  -- contribute thread and the timer if running
  return $ Notifier comunicate $ kill p >> atomically (readTVar timer) >>= maybe (return ()) kill 

{-
----------------------------------- testing -----------------------------------------
-- launch a stone and wait
lsw :: (Configuration -> IO Notifier) -> Property
lsw noti = monadicIO $ do
  l <- run $ mkTempDir
  let f = l </> "prova"
  t <- pick $ choose (0,4)
  let c = Configuration l t ((==) f)
  n <- noti c
  run $ waitIO n >>= print  
 -}
