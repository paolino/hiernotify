{-# LANGUAGE TupleSections#-}

-- | Abstract notifier definitions.
module System.Hiernotify (Difference, DifferenceP (..), Configuration (..), Notifier (..) ) where

import Data.Monoid (Monoid (..), mempty, mappend)
import Data.List ((\\), nub, intersect)
import Data.Int

-- | Difference datatype containing a difference as three sets of paths. This datatype is the core content of a notification of changes in a hierarchy.
data DifferenceP a = DifferenceP {
  created :: [a], -- ^ Files appeared
  deleted :: [a], -- ^ Files disappeared
  modified :: [a] -- ^ Files modified
  } deriving (Show, Eq)


instance Functor DifferenceP where
  fmap f (DifferenceP xs ys zs) = DifferenceP (map f xs) (map f ys) (map f zs)

type Difference = DifferenceP FilePath



-- half correct instance. It forces files which have been deleted and created to be marked as modifications. It's not correct as a delete after a create is not a modification. But correcting this bug involves mostly comparing timestamps correctly, because it can happen inside one element of the mappend.
instance Eq a => Monoid (DifferenceP a) where
  DifferenceP n d m `mappend` DifferenceP n' d' m' = let
    mm = nub $ m ++ m'
    nn = nub $ n ++ n'
    dd = nub $ d ++ d' 
    in DifferenceP ((nn \\ dd) \\ mm) ((dd \\ nn) \\ mm) (nub $ mm ++ intersect nn dd)
  mempty = DifferenceP [] [] []

  
-- | Configuration for notifiers. Minimal configuration to build a notifier.
data Configuration = Configuration 
  { top :: FilePath             -- ^ directory at the top of the hierarchy under control
  , silence :: Int64              -- ^ minimum time lapse in seconds where nothing changes before a difference is released
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


    



