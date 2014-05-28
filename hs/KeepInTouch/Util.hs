module KeepInTouch.Util
( todayIO
) where

import Data.Time.Clock(getCurrentTime,utctDay)
import Data.Time.Calendar(Day)

-- | Today's date
todayIO :: IO Day
todayIO = fmap utctDay getCurrentTime
