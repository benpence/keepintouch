module KeepInTouch.Entry
( Entry(..)
) where

import Data.Time.Calendar(Day)

data Entry = Entry
    { interval      :: !Integer   -- ^ How often to contact these people
    , lastContacted :: !Day       -- ^ Last day these people were contacted
    , names         :: ![String]  -- ^ Names of people to contact
    } deriving (Eq, Ord, Show)
