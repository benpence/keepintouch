module KeepInTouch.Interface
(
  Interface(..)
, Problem(..)
) where

import KeepInTouch.Entry(Entry)

class Interface a where
    entriesIO   :: a -> IO [Entry]
    newPerson   :: a -> String -> IO (Maybe Entry)
    replaceData :: a -> [Entry] -> IO (Maybe Problem)
    scheduled   :: a -> [Entry] -> IO (Maybe Problem)
    

data Problem = FileError String
             | Usage
