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
