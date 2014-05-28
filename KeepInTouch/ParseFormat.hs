module KeepInTouch.ParseFormat
(
  -- 
  Parser
, plaintextParser

  -- 
, Formatter
, plaintextFormatter
) where

import Data.Char(isDigit,isSpace)
import Data.List(intercalate)
import Data.Maybe(fromJust,isJust)
import Data.Time.Format(formatTime,parseTime)
import System.Locale(defaultTimeLocale)

import KeepInTouch.Type(Entry(..))

-- | Transforms a String into Entries.
-- JSON, XML, or just newline-delimited lines are examples of input formats
type Parser = String -> [Entry]

-- | Transforms Entries into a String.
-- JSON, XML, or just newline-delimited lines are examples of output formats
type Formatter = [Entry] -> String

dateString = "%Y/%m/%d"

-- | Parse entries in format:
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

plaintextParser' (iLine : next@(dLine : name : rest)) =
  let
    hasInterval = all isDigit iLine

    date = parseTime defaultTimeLocale dateString dLine
    hasDate = isJust date

    isBlank = all isSpace
    hasName = not $ all isSpace name

    -- Consume names until blank line
    (names', rest') = break isBlank rest
    entry = Entry
        { interval      = read iLine
        , lastContacted = fromJust date
        , names         = name : names'
        }
  in
    if hasName && hasInterval && hasDate
    then entry : plaintextParser' rest'
    else plaintextParser' next
plaintextParser' _ = []

-- | Format entries into:
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
    format e = intercalate "\n" $ map ($ e) [intervalFormat, dateFormat, namesFormat]

    intervalFormat = show . interval
    dateFormat     = formatTime defaultTimeLocale dateString . lastContacted
    namesFormat    = intercalate "\n" . names
  in
    intercalate "\n\n" . map format
