module KeepInTouch.Schedule
(
  --
  Weight(..)
, defaultWeight

, Backlog(..)

, Shuffle(..)
) where

import Data.Function(on)
import Data.List(sort)
import Data.Time.Calendar(Day,addDays)
import System.Random(randoms,RandomGen)

import KeepInTouch.Type(Entry(..),Scheduler(..))
import KeepInTouch.Util(shuffle)

data Weight a = Weight
    { weight    :: Float
    , generator :: a
    }

-- Default weight 
defaultWeight :: Float
defaultWeight = 0.25

-- Entries sorted by lastContacted + a portion of the interval where the portion
--   of the interval will be randomly in the range:
--
--   interval -/+ (weight * interval)
--
instance (RandomGen a) => Scheduler (Weight a) where
    schedule (Weight weight generator) =
      let
        -- Pair with random values
        percentages = zip (randoms generator)

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

instance Scheduler Backlog where
    -- Entries due (lastContacted + interval) before today
    schedule (Backlog thisDay) =
      let
        overdue e = addDays (interval e) (lastContacted e)
        sortedWithOverdue = sort . map (\e -> (overdue e, e))

        beforeToday = fst . break ((thisDay <=) . fst)

        toEntries = map snd
      in
        toEntries . beforeToday . sortedWithOverdue

data Shuffle a = Shuffle a

instance (RandomGen a) => Scheduler (Shuffle a) where
    schedule (Shuffle gen) = shuffle gen
