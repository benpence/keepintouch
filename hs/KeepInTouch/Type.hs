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
    -- | Parse the input and deliver the entries
    entriesIO   :: a -> IO [Entry]

    -- | When marking unrecognized name as contacted today, ask the user for an
    -- interval to create a new Entry
    newPerson   :: a -> String -> IO (Maybe Entry)

    -- | After adding or updating ant Entry, store the new data persistently
    replaceData :: a -> [Entry] -> IO (Maybe Problem)

    -- | Present the entries to the user after scheduling them
    scheduled   :: a -> [Entry] -> IO (Maybe Problem)

data Problem = Fail IOError | Usage

class Scheduler a where
    schedule :: a -> [Entry] -> [Entry]
