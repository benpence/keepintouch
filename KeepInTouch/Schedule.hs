module KeepInTouch.Schedule
(
  --
  Policy(..)
, Weight
, Backlog
) where

import Data.Function(on)
import Data.List(sort)
import Data.Time.Calendar(Day,addDays)

import KeepInTouch.Entry(Entry(..))

class Policy a where
    schedule :: a -> [Entry] -> [Entry]

data Weight = Weight
    { weight    :: Float
    , generator :: [Float]
    }

-- Entries sorted by lastContacted + a portion of the interval where the portion
--   of the interval will be randomly in the range:
--
--   interval -/+ (weight * interval)
--
instance Policy Weight where
    schedule (Weight weight generator) =
      let
        -- Pair with random values
        percentages = zip generator

        -- lastContacted + (1 - weight) * interval +/- weight * random
        weightedValue :: (Float, Entry) -> (Day, Entry)
        weightedValue (randomPercent, entry) =
          let
            intervalF = fromIntegral $ interval entry

            normalWeighted = intervalF * (1 - weight)
            randomWeighted = intervalF * weight * 2 * randomPercent

            intervalDays = round $ normalWeighted + randomWeighted
            startDay = lastContacted entry
          in
            (addDays intervalDays startDay, entry)
      in
        -- lastContacted with random percentage of interval
        map snd . sort . map weightedValue . percentages

data Backlog = Backlog
    { today :: Day
    }

instance Policy Backlog where
    -- Entries due (lastContacted + interval) before today
    schedule (Backlog today) =
      let
        overdue e = addDays (interval e) (lastContacted e)
        overdues = map (\e -> (overdue e, e))
        sortedEntries = map snd . sort . overdues
      in
        filter ((<= today) . lastContacted) . sortedEntries
