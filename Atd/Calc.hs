module Atd.Calc (
    atTime
,   messageScript
,   Script
,   Recipient
) where

import System.Locale (TimeLocale)
import Data.Time.Format (FormatTime, formatTime)
import qualified Text.ShellEscape as SE
import qualified Data.ByteString.Char8 as BS

-- | Return a time for passing to at
atTime :: FormatTime t => TimeLocale -> t -> [String]
atTime locale t = [time, date]
    where   date = formatTime locale "%Y-%m-%d" t
            time = formatTime locale "%H:%M" t


type Recipient = String
type Subject = String
type Body = String
type Script = String

-- | Return an at script that sends a fixed email to a fixed recipient
messageScript :: Recipient -> Subject -> Body -> Script

messageScript to subj body =
    "echo " ++ message ++ " | mail -s " ++ subject ++ " " ++ recipient
    where
        message = escape body
        subject = escape subj
        recipient = escape to


escape :: String -> String
escape s = BS.unpack $ SE.bytes $ SE.sh $ BS.pack s
