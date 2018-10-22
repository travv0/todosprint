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
import qualified Data.Text                     as T

taskList :: [Entity Task] -> [Entity Task] -> Bool -> Maybe TimeOfDay -> Widget
taskList tasks postponedTasks detailed estimatedToc = do
  (Entity userId user) <- requireAuth
  currUtcTime          <- liftIO getCurrentTime
  let mUserTz = userTimeZone user
  let formattedEstimatedToc = maybe
        ""
        (formatTime defaultTimeLocale "Estimated Completion Time: %l:%M %p")
        estimatedToc

  utcTime <- liftIO getCurrentTime
  let today = localDay $ fromMaybe (utcToLocalTime utc utcTime) $ utcToUserTime
        utcTime
        user
  allTasks <- handlerToWidget $ runDB $ getTasks userId []

  let userTz = fromMaybe utc $ userTimeZone user

  -- get current time in user's time zone
  currentTime <- liftIO $ utctDayTime <$> getCurrentTime
  let currentTime'            = timeToTimeOfDay currentTime
  let (dayAdj, currentTime'') = utcToLocalTimeOfDay userTz currentTime'
  let curTime                 = timeOfDayToTime currentTime''

  let mUserDueTime            = userDueTime user

  -- get due time in user's time zone
  let dTime = case mUserDueTime of
        Just dt -> do
          let dt'       = timeToTimeOfDay $ utctDayTime dt
          let (_, dt'') = utcToLocalTimeOfDay userTz dt'
          timeOfDayToTime dt''
        Nothing -> currentTime

  -- use those to calulate how many minutes are left
  let timeOfDay = timeToTimeOfDay $ picosecondsToDiffTime
        (diffTimeToPicoseconds dTime - diffTimeToPicoseconds curTime)
  let mins = (todHour timeOfDay * 60) + todMin timeOfDay

  deps <- handlerToWidget $ runDB (selectList [] [])

  let todaysTasks = daysList today mins deps allTasks

  tasksDeps <- handlerToWidget $ sequence $ map
    (\t -> if M.isNothing (taskPostponeTime $ entityVal t)
      then return False
      else
        (<) 1
          <$> (   L.length
              <$> (L.intersect <$> taskAndDependencies t <*> pure todaysTasks)
              )
    )
    tasks
  let tasksHasDeps = zip tasks tasksDeps
  $(widgetFile "tasks")

getHomeR :: Handler Html
getHomeR = do
  setUltDestCurrent
  userId         <- requireAuthId
  tasks          <- runDB $ getTasks userId [Asc TaskDueDate]
  utcTime        <- liftIO getCurrentTime
  postponedTasks <- postponedTaskList utcTime tasks
  defaultLayout $ do
    setTitle "Manage Tasks"
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
      (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 4))
    $   DueTime
    <$> areq timeField (bfs ("When would you like to work until?" :: Text)) mdt
    <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

userTimeZone :: User -> Maybe TimeZone
userTimeZone user = fmap minutesToTimeZone $ userDueTimeOffset user

utcToUserTime :: UTCTime -> User -> Maybe LocalTime
utcToUserTime time user = do
  userTz <- userTimeZone user
  return $ utcToLocalTime userTz time

postponedTaskList :: UTCTime -> [Entity Task] -> Handler [Entity Task]
postponedTaskList currUtcTime tasks = tasksAndDependents
  $ filter postponed tasks
 where
  postponed (Entity _ task) = case taskPostponeTime task of
    Nothing     -> False
    Just ppTime -> currUtcTime < ppTime

tasksAndDependents :: [Entity Task] -> Handler [Entity Task]
tasksAndDependents tasks = tasksAndDependents' tasks []

tasksAndDependents' :: [Entity Task] -> [Entity Task] -> Handler [Entity Task]
tasksAndDependents' []    postponedTasks = return postponedTasks
tasksAndDependents' tasks postponedTasks = do
  tad               <- taskAndDependents (L.head tasks)
  newPostponedTasks <- tasksAndDependents' (L.tail tasks) tad
  return $ postponedTasks ++ newPostponedTasks

taskAndDependents :: Entity Task -> Handler [Entity Task]
taskAndDependents task = do
  dependents <- runDB $ selectList
    [ TaskDependencyDependsOnTaskId ==. entityKey task
    , TaskDependencyDeleted ==. False
    ]
    []
  let tdIds = map (\(Entity _ td) -> taskDependencyTaskId td) dependents
  dependentEntities <- runDB $ mapM
    (\tdId -> selectList
      [TaskId ==. tdId, TaskDeleted ==. False, TaskDone ==. False]
      []
    )
    tdIds
  more <- sequence $ map taskAndDependents $ L.concat dependentEntities
  return $ task : L.concat more

taskAndDependencies :: Entity Task -> Handler [Entity Task]
taskAndDependencies task = do
  dependencies <- runDB $ selectList
    [TaskDependencyTaskId ==. entityKey task, TaskDependencyDeleted ==. False]
    []
  let tdIds =
        map (\(Entity _ td) -> taskDependencyDependsOnTaskId td) dependencies
  dependencyEntities <- runDB $ mapM
    (\tdId -> selectList
      [TaskId ==. tdId, TaskDeleted ==. False, TaskDone ==. False]
      []
    )
    tdIds
  more <- sequence $ map taskAndDependencies $ L.concat dependencyEntities
  return $ L.nub $ task : L.concat more

getResetDueTimeR :: Handler Html
getResetDueTimeR = do
  (Entity userId user) <- requireAuth
  runDB $ update userId [UserDueTime =. Nothing]
  redirect TodayR

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
      let tasks = daysList today mins deps tasks'

      -- calculate estimated time of completion
      let estimatedToc = if null tasks
            then Nothing
            else Just $ estimateTimeOfCompletion tasks currentTime''

      postponedTasks <- postponedTaskList utcTime tasks
      defaultLayout $ do
        setTitle "Today's Tasks"
        $(widgetFile "work-message")
        taskList tasks postponedTasks False estimatedToc
    Nothing -> do
      (widget, enctype) <- generateFormPost $ dueTimeForm tzOffsetId Nothing
      defaultLayout $ do
        setTitle "Set Time"
        setTimeWidget widget enctype tzOffsetId

estimateTimeOfCompletion :: [Entity Task] -> TimeOfDay -> TimeOfDay
estimateTimeOfCompletion tasks tod = TimeOfDay hours mins (todSec tod)
 where
  taskMins = timeToComplete tasks
  mins     = (todMin tod + taskMins) `mod` 60
  hours    = (todHour tod + ((todMin tod + taskMins) `div` 60)) `mod` 24

formatPostponeTime :: TimeZone -> Entity Task -> String
formatPostponeTime userTz (Entity _ task) = case taskPostponeTime task of
  Just time -> formatTime defaultTimeLocale "%l:%M %p" $ userTime time
  Nothing   -> ""
  where userTime time = utcToLocalTime userTz time

setTimeWidget :: Widget -> Enctype -> Text -> Widget
setTimeWidget widget enctype tzOffsetId = do
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
    Just dd -> case taskPostponeDay t of
      Just ppd -> dd <= d && ppd <= d
      Nothing  -> dd <= d
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
  | allTasks == reducedTasks
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
  :: Day -> Int -> [Entity TaskDependency] -> [Entity Task] -> [Entity Task]
daysList day min dependencies tasks =
  ( sortTasks dependencies
    . fillInGaps min (dueByDay day tasks)
    . reduceLoad day min
    . dueByDay day
    )
    tasks

postMarkDoneR :: TaskId -> Handler Html
postMarkDoneR taskId = do
  setUltDestReferer
  task    <- runDB $ get taskId
  utcTime <- liftIO getCurrentTime
  runDB $ update taskId [TaskDone =. True, TaskDoneTime =. Just utcTime]
  (Entity userId user) <- requireAuth
  let userTzOffset = fromMaybe 0 $ userDueTimeOffset user
  let cdate =
        localDay $ utcToLocalTime (minutesToTimeZone userTzOffset) utcTime
  runDB $ case task of
    Just t -> case incrementDueDate cdate t of
      Just t2 -> do
        newTaskId <- insert
          $ t2 { taskPostponeDay = Nothing, taskPostponeTime = Nothing }
        deps <- selectList [TaskDependencyTaskId ==. taskId] []
        depd <- selectList [TaskDependencyDependsOnTaskId ==. taskId] []
        mapM
          (\(Entity _ dep) -> insert $ TaskDependency
            newTaskId
            (taskDependencyDependsOnTaskId dep)
            False
          )
          deps
        mapM
          (\(Entity _ dep) ->
            insert $ TaskDependency (taskDependencyTaskId dep) newTaskId False
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

getTodayDepsR :: TaskId -> Handler Html
getTodayDepsR taskId = do
  setUltDestCurrent

  (Entity userId user) <- requireAuth
  mTask                <- runDB $ getEntity taskId

  case mTask of
    Just task -> do
      utcTime <- liftIO getCurrentTime
      let today =
            localDay $ fromMaybe (utcToLocalTime utc utcTime) $ utcToUserTime
              utcTime
              user
      let userTz = fromMaybe utc $ userTimeZone user

      case taskPostponeTime $ entityVal task of
        Just ppt -> do
          -- get current time in user's time zone
          currentTime <- liftIO $ utctDayTime <$> getCurrentTime
          let currentTime'            = timeToTimeOfDay currentTime
          let (dayAdj, currentTime'') = utcToLocalTimeOfDay userTz currentTime'
          let curTime                 = timeOfDayToTime currentTime''

          -- get due time in user's time zone
          let ppt'                    = timeToTimeOfDay $ utctDayTime ppt
          let (_, ppt'')              = utcToLocalTimeOfDay userTz ppt'
          let ppTime                  = timeOfDayToTime ppt''

          -- use those to calulate how many minutes are left
          let timeOfDay = timeToTimeOfDay $ picosecondsToDiffTime
                (diffTimeToPicoseconds ppTime - diffTimeToPicoseconds curTime)
          let mins         = (todHour timeOfDay * 60) + todMin timeOfDay

          let mUserDueTime = userDueTime user
          todayMins <- case mUserDueTime of
            Just dt -> do
               -- get due time in user's time zone
              let dt'       = timeToTimeOfDay $ utctDayTime dt
              let (_, dt'') = utcToLocalTimeOfDay userTz dt'
              let dTime     = timeOfDayToTime dt''

              -- use those to calulate how many minutes are left
              let timeOfDay = timeToTimeOfDay $ picosecondsToDiffTime
                    (diffTimeToPicoseconds dTime - diffTimeToPicoseconds curTime
                    )
              return $ (todHour timeOfDay * 60) + todMin timeOfDay
            Nothing -> return mins

          deps     <- runDB (selectList [] [])

          allTasks <- runDB $ getTasks userId []

          let todaysTasks = daysList today todayMins deps allTasks

          tasks' <- taskAndDependencies task
          let tasks'' = L.delete task tasks'

          let tasks =
                daysList today mins deps $ tasks'' `L.intersect` todaysTasks

          case tasks of
            [] -> redirect TodayR
            _  -> do
              postponedTasks <- postponedTaskList utcTime tasks
              defaultLayout $ taskList tasks postponedTasks False Nothing
        Nothing -> redirect TodayR
    Nothing -> redirect TodayR

getTasks userId =
  selectList [TaskUserId ==. userId, TaskDone ==. False, TaskDeleted ==. False]
