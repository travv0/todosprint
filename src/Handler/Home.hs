{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Database.Persist.Sql
import           Import
import qualified Data.Graph                    as G
import           Priority
import           Data.Time
import           Data.Time.Calendar.WeekDate
import qualified Data.List                     as L
import           RepeatInterval
import           Yesod.Form.Bootstrap3
import qualified Data.Maybe                    as M
import           Text.Julius                    ( RawJS(..) )
import           Control.Monad

taskList :: [Entity Task] -> Widget
taskList tasks = $(widgetFile "tasks")

getHomeR :: Handler Html
getHomeR = do
  setUltDestCurrent
  userId <- requireAuthId
  tasks  <- runDB $ getTasks userId [Asc TaskDueDate]
  defaultLayout $ do
    setTitle "Your Tasks"
    $(widgetFile "homepage")

sortTasks' :: [Entity Task] -> [Entity TaskDependency] -> [G.SCC (Entity Task)]
sortTasks' tasks dependencies = G.stronglyConnComp tasksDeps
 where
  tasksDeps = map
    (\etask@(Entity taskId _) ->
      ( etask
      , taskId
      , map
        (\(Entity _ td) -> (taskDependencyDependsOnTaskId td))
        (filter (\(Entity _ td) -> (taskDependencyTaskId td == taskId))
                dependencies
        )
      )
    )
    tasks

sortTasks :: [Entity TaskDependency] -> [Entity Task] -> [Entity Task]
sortTasks dependencies tasks =
  concat $ map G.flattenSCC $ sortTasks' tasks dependencies

highWeight = 99999
mediumWeight = lowWeight * 2
lowWeight = 7
highOverdueMod = lowOverdueMod * 3
mediumOverdueMod = lowOverdueMod * 2
lowOverdueMod = 1
weight :: Day -> Task -> Integer
weight date task = case taskPriority task of
  High -> highWeight + case taskDueDate task of
    Just dd -> weightMod dd highOverdueMod
    Nothing -> 0
  Medium -> mediumWeight + case taskDueDate task of
    Just dd -> weightMod dd mediumOverdueMod
    Nothing -> 0
  Low -> lowWeight + case taskDueDate task of
    Just dd -> weightMod dd lowOverdueMod
    Nothing -> 0
  None -> 0
  where weightMod dd modifier = (date `diffDays` dd) * modifier

timeToComplete :: [Entity Task] -> Int
timeToComplete = foldr (\(Entity _ t) x -> x + taskDuration t) 0

data DueTime = DueTime
  { dueTime :: TimeOfDay
  } deriving Show

dueTimeForm :: Text -> Maybe TimeOfDay -> Form DueTime
dueTimeForm tzOffsetId mdt =
  renderBootstrap3
      (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 8))
    $   DueTime
    <$> areq timeField "When would you like to work until?" mdt
    <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

userTimeZone :: User -> Maybe TimeZone
userTimeZone user = fmap minutesToTimeZone $ userDueTimeOffset user

utcToUserTime :: UTCTime -> User -> Maybe LocalTime
utcToUserTime time user = do
  userTz <- userTimeZone user
  return $ utcToLocalTime userTz time

getTodayR :: Handler Html
getTodayR = do
  setUltDestCurrent
  (Entity userId user') <- requireAuth

  -- get current day in user's time zone
  currentDateTime       <- liftIO getCurrentTime
  let mCurrentUserDateTime = utcToUserTime currentDateTime user'

  -- get day of due time in user's time zone
  let mDueDateTime         = userDueTime user'
  let mDueUserDateTime     = mDueDateTime >>= (`utcToUserTime` user')

  -- if not the same day user set due time, reset it
  let shouldUpdateTime =
        (localDay <$> mDueUserDateTime) /= (localDay <$> mCurrentUserDateTime)
  runDB $ update
    userId
    [UserDueTime =. if shouldUpdateTime then Nothing else mDueDateTime]
  let user =
        if shouldUpdateTime then user' { userDueTime = Nothing } else user'

  let mUserDueTime = userDueTime user
  tzOffsetId <- newIdent
  case mUserDueTime of
    Just dt -> do
      let userTz = fromMaybe utc $ userTimeZone user

      let mLocalTod = fmap
            (utcToLocalTimeOfDay userTz . timeToTimeOfDay . utctDayTime)
            mUserDueTime
      (widget, enctype) <- generateFormPost
        $ dueTimeForm tzOffsetId (snd <$> mLocalTod)

      -- get current time in user's time zone
      currentTime <- liftIO $ utctDayTime <$> getCurrentTime
      let currentTime'            = timeToTimeOfDay currentTime
      let (dayAdj, currentTime'') = utcToLocalTimeOfDay userTz currentTime'
      let curTime                 = timeOfDayToTime currentTime''

      -- get due time in user's time zone
      let dt'                     = timeToTimeOfDay $ utctDayTime dt
      let (_, dt'')               = utcToLocalTimeOfDay userTz dt'
      let dTime                   = timeOfDayToTime dt''

      -- use those to calulate how many minutes are left
      let timeOfDay = timeToTimeOfDay $ picosecondsToDiffTime
            (diffTimeToPicoseconds dTime - diffTimeToPicoseconds curTime)
      let mins = (todHour timeOfDay * 60) + todMin timeOfDay

      tasks'  <- runDB $ getTasks userId []
      deps    <- runDB (selectList [] [])

      utcTime <- liftIO getCurrentTime
      let today = localDay $ utcToLocalTime userTz utcTime
      let tasks = daysList utcTime today mins deps tasks'

      -- calculate estimated time of completion
      let estimatedToc = if null tasks
            then Nothing
            else Just $ estimateTimeOfCompletion tasks currentTime''

      defaultLayout $ do
        setTimeWidget widget enctype tzOffsetId estimatedToc
        $(widgetFile "tasks")
    Nothing -> do
      (widget, enctype) <- generateFormPost $ dueTimeForm tzOffsetId Nothing
      defaultLayout $ setTimeWidget widget enctype tzOffsetId Nothing

estimateTimeOfCompletion :: [Entity Task] -> TimeOfDay -> TimeOfDay
estimateTimeOfCompletion tasks tod = TimeOfDay hours mins (todSec tod)
 where
  taskMins = timeToComplete tasks
  mins     = (todMin tod + taskMins) `mod` 60
  hours    = (todHour tod + ((todMin tod + taskMins) `div` 60)) `mod` 24

setTimeWidget :: Widget -> Enctype -> Text -> Maybe TimeOfDay -> Widget
setTimeWidget widget enctype tzOffsetId estimatedToc = do
  let formattedEstimatedToc = maybe
        ""
        (formatTime defaultTimeLocale "Estimated Completion Time: %l:%M %p")
        estimatedToc
  $(widgetFile "set-time")

postTodayR :: Handler Html
postTodayR = do
  (Entity userId user)     <- requireAuth
  tzOffsetId               <- newIdent
  ((res, widget), enctype) <- runFormPost $ dueTimeForm tzOffsetId Nothing
  case res of
    FormSuccess dt -> do
      userId <- requireAuthId

      let tzOffsetMins = fromMaybe 0 $ userDueTimeOffset user
      let userTz       = minutesToTimeZone tzOffsetMins
      utcTime <- liftIO getCurrentTime
      let userTime             = utcToLocalTime userTz utcTime

      let (dayAdj, utcDueTime) = localToUTCTimeOfDay userTz $ dueTime dt

      runDB $ update
        userId
        [ UserDueTime
            =. Just
                 (UTCTime (addDays dayAdj (localDay userTime))
                          (timeOfDayToTime utcDueTime)
                 )
        ]
      redirect TodayR
    _ -> do
      setMessage "Error updating time"
      redirect TodayR

dueByDay :: Day -> [Entity Task] -> [Entity Task]
dueByDay day = filter (dueByOrBeforeDay day)
 where
  dueByOrBeforeDay d (Entity _ t) = case taskDueDate t of
    Just dd -> dd <= d
    Nothing -> True

today :: IO Day
today = localDay . zonedTimeToLocalTime <$> getZonedTime

utcToday :: IO Day
utcToday = utctDay <$> getCurrentTime

reduceLoad :: Day -> Int -> [Entity Task] -> [Entity Task]
reduceLoad date min tasks
  | timeToComplete tasks > min && not (allHighPriority tasks)
  = reduceLoad date min $ L.tail $ L.sortBy
    (\et1@(Entity _ t1) et2@(Entity _ t2) ->
      lowestWeight et1 et2 <> (taskDuration t2 `compare` taskDuration t1)
    )
    tasks
  | otherwise
  = tasks
 where
  lowestWeight (Entity _ t1) (Entity _ t2) =
    weight date t1 `compare` weight date t2
  allHighPriority = foldr (\(Entity _ t) b -> b && taskPriority t == High) True

fillInGaps :: Int -> [Entity Task] -> [Entity Task] -> [Entity Task]
fillInGaps mins allTasks reducedTasks
  | null allTasks
  = reducedTasks
  | length reducedTasks == length allTasks
  = reducedTasks
  | timeToComplete reducedTasks
    + L.minimum (map (\(Entity _ t) -> taskDuration t) potTasks)
    > mins
  = reducedTasks
  | otherwise
  = let sortedPot = sortBy highestPriorityThenShortestLength potTasks
    in  fillInGaps
          mins
          (L.tail sortedPot)
          (if timeToComplete reducedTasks
               + taskDuration (getTaskFromEntity $ L.head sortedPot)
               < mins
            then L.head sortedPot : reducedTasks
            else reducedTasks
          )
 where
  potTasks =
    filter (\(Entity _ t) -> not $ taskDone t) allTasks L.\\ reducedTasks
  highestPriorityThenShortestLength (Entity _ a) (Entity _ b) =
    taskPriority b
      `compare` taskPriority a
      <>        taskDuration a
      `compare` taskDuration b
  getTaskFromEntity (Entity _ t) = t

daysList
  :: UTCTime
  -> Day
  -> Int
  -> [Entity TaskDependency]
  -> [Entity Task]
  -> [Entity Task]
daysList currUtcTime day min dependencies tasks =
  ( sortTasks dependencies
    . fillInGaps min (dueByDay day tasks)
    . reduceLoad day min
    . dueByDay day
    )
    tasks
 where
  notPostponed (Entity _ t) = case taskPostponeTime t of
    Just tppTime -> currUtcTime > tppTime
    Nothing      -> True

postMarkDoneR :: TaskId -> Handler Html
postMarkDoneR taskId = do
  setUltDestReferer
  task <- runDB $ get taskId
  runDB $ update taskId [TaskDone =. True]
  utcTime              <- liftIO getCurrentTime
  (Entity userId user) <- requireAuth
  let userTzOffset = fromMaybe 0 $ userDueTimeOffset user
  let cdate =
        localDay $ utcToLocalTime (minutesToTimeZone userTzOffset) utcTime
  runDB $ case task of
    Just t -> case incrementDueDate cdate t of
      Just t2 -> do
        newTaskId <- insert t2
        deps      <- selectList [TaskDependencyTaskId ==. taskId] []
        depd      <- selectList [TaskDependencyDependsOnTaskId ==. taskId] []
        mapM
          (\(Entity _ dep) -> insert
            $ TaskDependency newTaskId (taskDependencyDependsOnTaskId dep)
          )
          deps
        mapM
          (\(Entity _ dep) ->
            insert $ TaskDependency (taskDependencyTaskId dep) newTaskId
          )
          depd
      Nothing -> redirectUltDest HomeR
    Nothing -> redirectUltDest HomeR
  redirectUltDest HomeR

incrementDueDate :: Day -> Task -> Maybe Task
incrementDueDate cdate task = case taskDueDate task of
  Just dd -> case taskRepeat task of
    Just (OnWeekdays wkdays CompletionDate) -> do
      let (_, _, wkdayNum) = toWeekDate cdate
      let nextWkday        = next $ weekdayFromInt wkdayNum
      if nextWkday `elem` wkdays
        then Just task { taskDueDate = Just (addDays 1 cdate) }
        else incrementDueDate (addDays 1 cdate) task
    Just (OnWeekdays wkdays DueDate) -> do
      let (_, _, wkdayNum) = toWeekDate dd
      let potWkdays = map weekdayFromInt [wkdayNum + 1 .. wkdayNum + 7]
      let matches          = map (`elem` wkdays) potWkdays
      let offset           = fmap (+ 1) (True `L.elemIndex` matches)
      case offset of
        Nothing -> Nothing
        Just o  -> Just task { taskDueDate = Just (addDays (toInteger o) dd) }
    Just (Days ivl CompletionDate) ->
      Just task { taskDueDate = Just (addDays ivl cdate) }
    Just (Weeks ivl CompletionDate) ->
      Just task { taskDueDate = Just (addDays (ivl * 7) cdate) }
    Just (Months ivl CompletionDate) ->
      Just task { taskDueDate = Just (addGregorianMonthsClip ivl cdate) }
    Just (Years ivl CompletionDate) ->
      Just task { taskDueDate = Just (addGregorianYearsClip ivl cdate) }
    Just (Days ivl DueDate) ->
      Just task { taskDueDate = Just (addDays ivl dd) }
    Just (Weeks ivl DueDate) ->
      Just task { taskDueDate = Just (addDays (ivl * 7) dd) }
    Just (Months ivl DueDate) ->
      Just task { taskDueDate = Just (addGregorianMonthsClip ivl dd) }
    Just (Years ivl DueDate) ->
      Just task { taskDueDate = Just (addGregorianYearsClip ivl dd) }
    Nothing -> Nothing
  Nothing -> Nothing

getTasks userId = selectList [TaskUserId ==. userId, TaskDone ==. False]
