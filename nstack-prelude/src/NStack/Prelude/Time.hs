module NStack.Prelude.Time where

import Data.AffineSpace ((.+^))
import Data.Thyme (UTCTime, formatTime, parseTime)
import Data.Thyme.Clock (fromSeconds)
import System.Locale (defaultTimeLocale, iso8601DateFormat)

addDays :: Integer -> UTCTime -> UTCTime
addDays days curTime = curTime .+^ fromSeconds (60*60*24*days :: Integer)

timeToIso8601 :: UTCTime -> String
timeToIso8601 = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))

timeToUnix :: UTCTime -> String
timeToUnix = formatTime defaultTimeLocale unixTimeFormat

timeFromUnix :: String -> Maybe UTCTime
timeFromUnix = parseTime defaultTimeLocale unixTimeFormat

unixTimeFormat :: String
unixTimeFormat = "%s"

