{-# LANGUAGE TupleSections #-}
module System.Hiernotify.Controller  where


import System.Time (ClockTime)
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
import System.Hiernotify (Notifier (..), Difference (..))
import System.Directory (getModificationTime, doesDirectoryExist, getDirectoryContents)
import System.FilePath (normalise, (</>))

update :: [FilePath] -> Difference -> [FilePath]
update ps (Difference ns ds ms) = nub $ (ps ++ ns ++ ms) \\ ds


-- | An abstract Controller. Parametrized on its configuration, it runs in its thread 
data Controller = Controller {base :: [FilePath] , step :: NextDiff}

-- | Infinite waiters for differences 
newtype NextDiff = NextDiff (IO (Difference,NextDiff)) 

-- | Get all paths under a directory
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


-- | make an Notifier given a Controller and the Notifier configuration
mkNotifier :: Int -> Controller -> IO Notifier
mkNotifier s (Controller ps0 nd0) = do
  ermes <- newTVarIO (mempty,ps0)
  timer <- newTVarIO Nothing -- timer
  let delta = s * 10 ^ (6 :: Int)
  let comunicate = atomically $ do 
        readTVar timer >>= maybe (return ()) (void . T.wait) 
        (d,p) <- readTVar ermes
        when (d == mempty) retry
        writeTVar ermes (mempty,update p d)
        return (d,p)
  let contribute (NextDiff nd) = do
        (d,nd') <- nd 
        mt <- atomically $ readTVar timer 
        case mt of
          Nothing -> T.parallel (atomically $ writeTVar timer Nothing) delta >>= atomically . writeTVar timer . Just 
          Just t -> T.renewIO t $ delta
        atomically $ readTVar ermes >>= writeTVar ermes . first (`mappend` d)
        contribute nd'

  p <- forkIO $ contribute nd0
  return $ Notifier comunicate $ kill p >> atomically (readTVar timer) >>= maybe (return ()) kill 

