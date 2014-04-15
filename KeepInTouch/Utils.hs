module KeepInTouch.Utils
( todayIO
) where

import Data.Time.Clock(getCurrentTime,utctDay)
import Data.Time.Calendar(Day)

todayIO :: IO Day
todayIO = fmap utctDay getCurrentTime
