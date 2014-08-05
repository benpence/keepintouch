module KeepInTouch.Util
( todayIO
, shuffle
) where

import Data.Array(listArray,Array(..),(!),(//),bounds,elems)
import Data.Time.Clock(getCurrentTime,utctDay)
import Data.Time.Calendar(Day)
import System.Random(randomR,RandomGen)

-- | Today's date
todayIO :: IO Day
todayIO = fmap utctDay getCurrentTime

-- TODO: Find solution that does not require constructing an array
-- | Generate a random permutation of a list by moving already-randomly selected
--   elements outside of the new possible random selection range (the beginning)
shuffle :: (RandomGen g) => g -> [a] -> [a]
shuffle generator l = elems . fst $ foldr shuffle' firstAccum [minI..maxI]
  where
    -- TODO: How to construct array without providing upper bound?
    -- Convert list to array
    array        = listArray (1, length l) l
    (minI, maxI) = bounds array

    firstAccum = (array, randomR (minI, maxI) generator)

    -- Swap elements at indeces
    swap :: Array Int a -> Int -> Int -> Array Int a
    swap arr a b = (arr // [(a, arr ! b), (b, arr ! a)])

    -- Move random element to the beginning of the range
    shuffle' :: (RandomGen g) => Int -> (Array Int a, (Int, g)) -> (Array Int a, (Int, g))
    shuffle' replaceIndex (arr, (randomIndex, g)) = (newArr, newG)
      where
        newArr = swap arr replaceIndex randomIndex
        newG   = randomR (replaceIndex, maxI) g
