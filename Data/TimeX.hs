module Data.TimeX where

import Data.Time (UTCTime, formatTime, defaultTimeLocale, FormatTime, TimeZone, utcToLocalTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)


abbrevTime, keepDateAbbrevTime :: FormatTime t => t -> String
abbrevTime         = formatTime defaultTimeLocale "%T"
keepDateAbbrevTime = formatTime defaultTimeLocale "%F %T"

keepDateAbbrevTime' :: TimeZone -> UTCTime -> String
keepDateAbbrevTime' timeZone = keepDateAbbrevTime . utcToLocalTime timeZone

-- abbrevTimeRead :: String -> String
-- abbrevTimeRead = abbrevTime . read
