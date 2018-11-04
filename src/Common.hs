module Common where

import           Import
import           Data.Time

fixTaskPostponeTime :: User -> Task -> Maybe UTCTime
fixTaskPostponeTime u t = case taskDueDate t of
  Nothing      -> taskPostponeTime t
  Just localDd -> do
    userTz <- userTimeZone u
    utcPpt <- timeToTimeOfDay <$> utctDayTime <$> taskPostponeTime t
    let (_dayAdj, localPpt) = utcToLocalTimeOfDay userTz utcPpt
    Just $ localTimeToUTC userTz (LocalTime localDd localPpt)

userTimeZone :: User -> Maybe TimeZone
userTimeZone user = fmap minutesToTimeZone $ userDueTimeOffset user

utcToUserTime :: UTCTime -> User -> Maybe LocalTime
utcToUserTime time user = do
  userTz <- userTimeZone user
  return $ utcToLocalTime userTz time
