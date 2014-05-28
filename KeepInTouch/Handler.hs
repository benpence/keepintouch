module KeepInTouch.Handler
( contactHandler
, scheduleHandler
) where

import Data.Char(toUpper)
import Data.List(partition)
import Data.Maybe(fromMaybe)
import Text.Read(readMaybe)

import KeepInTouch.Type(Entry(..),Interface(..),Problem(..),Scheduler(..))
import KeepInTouch.Util(todayIO)

-- | Add or update when you contacted someone
-- NOTE: The order of the inputted entries will not be preserved
contactHandler :: (Interface a) => a -> String -> IO (Maybe Problem)
contactHandler interface name =
  let
    uppercase    = map toUpper
    containsName = elem (uppercase name) . map uppercase . names

    -- One entry contains name
    onOneMatch e es = do
        today <- todayIO
        let newEntries = e { lastContacted = today } : es
        replaceData interface $ newEntries

    -- 0 or more than 1 entries contain name
    onNotOneMatch es = do
        newPerson <- (newPerson interface) name
        case newPerson of
            Just p  -> replaceData interface $ p : es
            -- TODO: i18n
            Nothing -> return $ Just Usage
  in do
    entries <- entriesIO interface

    -- Seperate entries containing name from those that do not
    case partition containsName entries of
        ([entry], rest) -> onOneMatch entry rest
        _               -> onNotOneMatch entries

-- | Schedule entries, returning all or some of them in a different order
scheduleHandler :: (Interface a, Scheduler b) => a -> b -> IO (Maybe Problem)
scheduleHandler interface scheduler = do
    entries <- entriesIO interface
    let scheduledEntries = schedule scheduler $ entries
    scheduled interface $ scheduledEntries
    return Nothing
