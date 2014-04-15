module KeepInTouch.ParseFormat
(
  -- 
  Parser
, plaintextParser

  -- 
, Formatter
, plaintextFormatter
) where

import Data.Char(isDigit)
import Data.List(intercalate)
import Data.Maybe(fromJust,isJust)
import Data.Time.Format(formatTime,parseTime)
import KeepInTouch.Entry(Entry(..),interval,lastContacted,names)
import System.Locale(defaultTimeLocale)

type Parser = String -> [Entry]
type Formatter = [Entry] -> String

dateString = "%Y/%m/%d"

-- Parse entries in format:
--
--  interval
--  date
--  name1
--  name2
--  ...
--
--  interval
--  date
--  name1
--  name2
--  ...
plaintextParser :: Parser
plaintextParser = plaintextParser' . lines

plaintextParser' (iLine : next@(dLine : name@(_:_) : rest)) =
  let
    hasInterval = all isDigit iLine

    date = parseTime defaultTimeLocale dateString dLine
    hasDate = isJust date
  in
    if hasInterval && hasDate
    then
      let
        (names', rest') = break null rest
        entry = Entry
            { interval      = read iLine
            , lastContacted = fromJust date
            , names         = name : names'
            }
      in
        entry : plaintextParser' rest'
    else plaintextParser' next
plaintextParser' _ = []

-- Format entries into:
--
--  interval
--  date
--  name1
--  name2
--  ...
plaintextFormatter :: Formatter
plaintextFormatter =
  let
    format :: Entry -> String
    format e = unlines $ map ($ e) [intervalFormat, dateFormat, namesFormat]

    intervalFormat = show
    dateFormat     = formatTime defaultTimeLocale dateString . lastContacted
    namesFormat    = unlines . names
  in
    intercalate "\n\n" . map format
