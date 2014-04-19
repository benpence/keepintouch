module KeepInTouch.Type
(
  Entry(..)

, Interface(..)
, Problem(..)

, Scheduler(..)
) where

import Data.Time.Calendar(Day)
import System.IO.Error(IOError)

data Entry = Entry
    { interval      :: !Integer   -- ^ How often to contact these people
    , lastContacted :: !Day       -- ^ Last day these people were contacted
    , names         :: ![String]  -- ^ Names of people to contact
    } deriving (Eq, Ord, Show)

class Interface a where
    entriesIO   :: a -> IO [Entry]
    newPerson   :: a -> String -> IO (Maybe Entry)
    replaceData :: a -> [Entry] -> IO (Maybe Problem)
    scheduled   :: a -> [Entry] -> IO (Maybe Problem)

data Problem = Fail IOError | Usage

class Scheduler a where
    schedule :: a -> [Entry] -> [Entry]
