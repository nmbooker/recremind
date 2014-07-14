module Recremind.Scheduler (
    Reminder(..)
,   reminderSubj
,   reminderBody
) where

-- This contains the action that will actually build the reminder and schedule it


import Data.Hourglass (Duration(..), TimeInterval(..), DateTime, timePrint)

data Reminder = Reminder {
        programName     :: String
    ,   firstShowing    :: DateTime
    ,   timerPeriodDays :: Int          -- how far in advance we can set timer, in days
}


reminderSubj :: Reminder -> String
reminderSubj reminder = "TODO: Set timer to record " ++ (programName reminder)

reminderBody :: Reminder -> String
reminderBody reminder = unlines $
    [   "Program:        " ++ (programName reminder)
    ,   "When:           " ++ (timePrint "%d/%m/%Y %H:%M" $ firstShowing reminder)
    ]

hoursPerDay = 24

--whenToRemind :: Reminder -> LocalTime
--whenToRemind reminder = undefined

-- Consider using the 'hourglass' package to do arithmetic on times and dates
--howLongTo :: LocalTime -> LocalTime -> 


-- TODO Function to work out when to send the email given a 'now' time
-- TODO Function to schedule sending of the email (will be IO)
