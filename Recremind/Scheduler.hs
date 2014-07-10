module Recremind.Scheduler (
    Reminder(..)
,   reminderSubj
,   reminderBody
) where

-- This contains the action that will actually build the reminder and schedule it

data Reminder = Reminder {
        programName     :: String
    ,   firstShowing    :: LocalTime
    ,   timerPeriodDays :: Int
}


reminderSubj :: Reminder -> String
reminderSubj reminder = "TODO: Set timer to record " ++ (programName reminder)

reminderBody :: Reminder -> String
reminderBody reminder = unlines $
    [   "Program:        " ++ (programName reminder)
    ,   "When:           " ++ (firstShowing reminder)
    ]

--whenToRemind :: Reminder -> LocalTime
--whenToRemind reminder = undefined

-- Consider using the 'hourglass' package to do arithmetic on times and dates
--howLongTo :: LocalTime -> LocalTime -> 


-- TODO Function to work out when to send the email given a 'now' time
-- TODO Function to schedule sending of the email (will be IO)
