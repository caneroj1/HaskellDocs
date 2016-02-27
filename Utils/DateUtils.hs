module Utils.DateUtils where

import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime

currentUTC = getCurrentTime
currentTimeZone = getCurrentTimeZone

timeDiff :: UTCTime -> UTCTime -> String
timeDiff f l = show $ diffUTCTime l f

getTimeString :: TimeZone -> UTCTime -> String
getTimeString tzone utc = output
  where localTime          = utcToLocalTime tzone utc
        ymd                = toGregorian $ localDay localTime
        timeOfDayString    = formatTimeOfDay $ localTimeOfDay localTime
        output             = joinTuple ymd ++ "_" ++ timeOfDayString

formatTimeOfDay :: TimeOfDay -> String
formatTimeOfDay timeOfDay = output
  where minutes   = todMin timeOfDay
        hour      = todHour timeOfDay
        sec       = truncate $ todSec timeOfDay
        output    = joinTuple (hour, minutes, sec)

joinTuple :: (Show a, Show b, Show c) => (a, b, c) -> String
joinTuple (x, y, z) = show x ++ "-" ++ show y ++ "-" ++ show z


utcToLocal :: UTCTime -> IO LocalTime
utcToLocal utc = do
  tz     <- getCurrentTimeZone
  return $ utcToLocalTime tz utc

timeToString :: (FormatTime t) => t -> String
timeToString =
  formatTime defaultTimeLocale format
  where format = "%m/%d/%Y at %l %P"
