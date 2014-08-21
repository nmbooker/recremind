module Atd.Calc (
    atTime
) where

import System.Locale (TimeLocale)
import Data.Time.Format (FormatTime, formatTime)

-- | Return a time for passing to at
atTime :: FormatTime t => TimeLocale -> t -> [String]
atTime locale t = [time, date]
    where   date = formatTime locale "%Y-%m-%d" t
            time = formatTime locale "%H:%M" t

