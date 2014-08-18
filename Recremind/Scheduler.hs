module Recremind.Scheduler (
    Reminder(..)
,   reminderSubj
,   reminderBody
,   whenToRemind
) where

-- This contains the action that will actually build the reminder and schedule it

-- import Data.Hourglass (Duration(..), Hours(..), DateTime, timePrint, timeAdd)
import Data.Time
import System.Locale (defaultTimeLocale)

data Reminder = Reminder {
        programName     :: String    -- ^ name of program
    ,   channel         :: String    -- ^ name of broadcast channel
    ,   firstShowing    :: LocalTime  -- ^ time of first showing
    ,   timerPeriodDays :: Integer     -- ^ how far in advance we can set timer, in days
} deriving (Show)


reminderSubj :: Reminder
                -> String   -- ^ subject for reminder email
reminderSubj reminder = "TODO: Set timer to record " ++ (programName reminder)

reminderBody :: Reminder
                -> String   -- ^ body of reminder email
reminderBody reminder = unlines $
    [   "Program:        " ++ (programName reminder)
    ,   "Channel:        " ++ (channel reminder)
    ,   "When:           " ++ (formatTime defaultTimeLocale "%d/%m/%Y %H:%M" $ firstShowing reminder)
    ]


whenToRemind :: Reminder 
                -> LocalTime     -- ^ when to send the reminder
whenToRemind reminder =
    (timerPeriodDays reminder) `daysBefore` (firstShowing reminder)

addDaysToLocalTime :: Integer -> LocalTime -> LocalTime
addDaysToLocalTime nDays time =
    let newDay = addDays nDays (localDay time)
    in time { localDay = newDay }

d `daysBefore` t = addDaysToLocalTime (0 - d) t

