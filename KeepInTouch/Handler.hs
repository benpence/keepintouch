module KeepInTouch.Handler
( contactHandler
, scheduleHandler
) where

import Data.Char(toUpper)
import Data.List(partition)
import qualified Data.Map as Map
import Data.Maybe(fromMaybe)
import Text.Read(readMaybe)

import KeepInTouch.Entry(Entry,names,lastContacted)
import KeepInTouch.Interface(Interface,Problem(..),entriesIO,newPerson,replaceData,scheduled)
import KeepInTouch.Schedule(Policy(..))
import KeepInTouch.Utils(todayIO)

-- Add or update when you contacted someone
-- NOTE: The order of the inputted entries will not be preserved
contactHandler :: (Interface a) => a -> String -> IO (Maybe Problem)
contactHandler interface name =
  let
    uppercase    = map toUpper

    containsName = elem (uppercase name) . map uppercase . names
    extractMatch = partition containsName

    write        = replaceData interface

    -- One entry contains name
    onMatch e es = do
        today <- todayIO
        let newEntries = e { lastContacted = today } : es
        write newEntries

    -- 0 or more than 1 entries contain name
    onNoMatch es = do
        newPerson <- (newPerson interface) name
        case newPerson of
            Just p  -> write $ p : es
            Nothing -> return $ Just Usage
  in do
    entries <- entriesIO interface

    -- Find entry with name if it exists
    case extractMatch entries of
        ([entry], rest) -> onMatch entry rest
        _               -> onNoMatch entries

scheduleHandler :: (Interface a, Policy b) => a -> b -> IO (Maybe Problem)
scheduleHandler interface policy = do
    entries <- entriesIO interface
    let scheduledEntries = schedule policy $ entries
    scheduled interface $ scheduledEntries
