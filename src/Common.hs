{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE NoImplicitPrelude     #-}
module Common where

import           Import
import           Data.Time

userTimeZone :: User -> Maybe TimeZone
userTimeZone user = minutesToTimeZone <$> userDueTimeOffset user

userTimeZoneOrUtc :: User -> TimeZone
userTimeZoneOrUtc user = fromMaybe utc $ userTimeZone user

utcToUserTime :: UTCTime -> User -> Maybe LocalTime
utcToUserTime time user = do
  userTz <- userTimeZone user
  return $ utcToLocalTime userTz time

getToday :: Maybe User -> IO Day
getToday muser = do
    currTime <- liftIO getCurrentTime
    let tzOffsetMins = fromMaybe 0 $ muser >>= userDueTimeOffset
    let userTz = minutesToTimeZone tzOffsetMins
    let localTime = utcToLocalTime userTz currTime
    return (localDay localTime)