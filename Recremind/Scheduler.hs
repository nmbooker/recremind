module Recremind.Scheduler (
    Reminder(..)
,   reminderSubj
,   reminderBody
,   whenToRemind
) where

-- This contains the action that will actually build the reminder and schedule it

import Data.Hourglass (Duration(..), Hours(..), DateTime, timePrint, timeAdd)
import Data.Int (Int64)

data Reminder = Reminder {
        programName     :: String    -- ^ name of program
    ,   channel         :: String    -- ^ name of broadcast channel
    ,   firstShowing    :: DateTime  -- ^ time of first showing
    ,   timerPeriodDays :: Int64     -- ^ how far in advance we can set timer, in days
} deriving (Show)


reminderSubj :: Reminder
                -> String   -- ^ subject for reminder email
reminderSubj reminder = "TODO: Set timer to record " ++ (programName reminder)

reminderBody :: Reminder
                -> String   -- ^ body of reminder email
reminderBody reminder = unlines $
    [   "Program:        " ++ (programName reminder)
    ,   "Channel:        " ++ (channel reminder)
    ,   "When:           " ++ (timePrint "DD/MM/YYYY H:MI" $ firstShowing reminder)
    ]


whenToRemind :: Reminder 
                -> DateTime     -- ^ when to send the reminder
whenToRemind reminder =
    (timerPeriodDays reminder) `daysBefore` (firstShowing reminder)


numDays `daysBefore` time =
    time `timeAdd` nullDuration {
        durationHours = Hours ((- numDays) * hoursPerDay)
    }

hoursPerDay = 24
nullDuration = Duration 0 0 0 0
