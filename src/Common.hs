module Common where

import           Import
import           Data.Time

fixTaskPostponeTime :: User -> Task -> Maybe UTCTime
fixTaskPostponeTime u t = case taskDueDate t of
  Nothing      -> taskPostponeTime t
  Just localDd -> do
    let userTz = userTimeZoneOrUtc u
    utcPpt <- timeToTimeOfDay <$> utctDayTime <$> taskPostponeTime t
    let (_dayAdj, localPpt) = utcToLocalTimeOfDay userTz utcPpt
    Just $ localTimeToUTC userTz (LocalTime localDd localPpt)

userTimeZone :: User -> Maybe TimeZone
userTimeZone user = fmap minutesToTimeZone $ userDueTimeOffset user

userTimeZoneOrUtc :: User -> TimeZone
userTimeZoneOrUtc user = fromMaybe utc $ userTimeZone user

utcToUserTime :: UTCTime -> User -> Maybe LocalTime
utcToUserTime time user = do
  userTz <- userTimeZone user
  return $ utcToLocalTime userTz time
