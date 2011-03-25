{-# LANGUAGE TupleSections#-}

-- | Abstract notifier definitions.
module System.Hiernotify (Difference (..), Configuration (..), Notifier (..) ) where

import Data.Monoid (Monoid (..), mempty, mappend)
import Data.List ((\\), nub, intersect)

-- | Difference datatype containing a difference as three sets of paths. This datatype is the core content of a notification of changes in a hierarchy.
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

  
-- | Configuration for notifiers. Minimal configuration to build a notifier.
data Configuration = Configuration 
  { top :: FilePath             -- ^ directory at the top of the hierarchy under control
  , silence :: Int              -- ^ minimum time lapse in seconds where nothing changes before a difference is released
  , select :: FilePath -> Bool  -- ^ filter for file paths, positive must be included 
  }

-- | Abstract notifiers. A Notifier is an object controlling a hierarchy. 
--
-- Its difference method will block until a Difference is available and at least a time of peace has elapsed.
--
-- Reading a difference must result internally in deleting the difference and updating the list of paths. 
-- The list of paths read together with the difference is always the list of paths to which the difference will be applied.
data Notifier = Notifier 
  { difference :: IO (Difference,[FilePath]) -- ^ block until next difference 
  , stop :: IO ()  -- ^ stop the notification daemon 
  }


    



