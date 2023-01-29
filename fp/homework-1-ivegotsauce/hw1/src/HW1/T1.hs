module HW1.T1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import Numeric.Natural

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving Show

-- | Returns the day that follows the day of the week given as input.
nextDay :: Day -> Day
nextDay day = case day of
                Monday    -> Tuesday
                Tuesday   -> Wednesday
                Wednesday -> Thursday
                Thursday  -> Friday
                Friday    -> Saturday
                Saturday  -> Sunday
                Sunday    -> Monday

-- | Returns the day of the week after a given number of days has passed.
afterDays :: Natural -> Day -> Day
afterDays n day = if n == 0 then day else afterDays (n - 1) (nextDay day)

-- | Checks if the day is on the weekend.
isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- | Computes the number of days until Friday.
daysToParty :: Day -> Natural
daysToParty day = countDays 0 day

countDays :: Natural -> Day -> Natural
countDays acc Friday = acc
countDays acc day    = countDays (acc + 1) (nextDay day)
