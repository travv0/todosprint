{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Common where

import Data.Time (
    LocalTime,
    TimeZone,
    minutesToTimeZone,
    utc,
    utcToLocalTime,
 )
import Import

userTimeZone :: User -> Maybe TimeZone
userTimeZone user = minutesToTimeZone <$> userDueTimeOffset user

userTimeZoneOrUtc :: User -> TimeZone
userTimeZoneOrUtc user = fromMaybe utc $ userTimeZone user

utcToUserTime :: UTCTime -> User -> Maybe LocalTime
utcToUserTime time user = utcToLocalTime <$> userTimeZone user <*> pure time
