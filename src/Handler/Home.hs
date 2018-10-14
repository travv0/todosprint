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
import qualified Data.Time.Units               as TU
import qualified Data.List                     as L
import           RepeatInterval
import           Yesod.Form.Bootstrap3

taskList :: [Entity Task] -> Widget
taskList tasks = $(widgetFile "tasks")

getHomeR :: Handler Html
getHomeR = do
  userId <- requireAuthId
  tasks  <- runDB $ getTasks userId [Desc TaskId]
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
 where
  weightMod dd modifier =
    let divisor = (modifier * (date `diffDays` dd))
    in  (date `diffDays` dd)
        * modifier
        - (toInteger (taskDuration task) `div` case divisor of
            0 -> 1
            _ -> divisor
          )

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

timeToComplete :: [Entity Task] -> Int
timeToComplete = foldr (\(Entity _ t) x -> x + taskDuration t) 0

data DueTime = DueTime
  { dueTime :: TimeOfDay
  , tzOffset :: Int
  } deriving Show

dueTimeForm :: Form DueTime
dueTimeForm =
  renderBootstrap3
      (BootstrapHorizontalForm (ColXs 0) (ColXs 4) (ColXs 0) (ColXs 8))
    $   DueTime
    <$> areq timeField "When would you like to work until?" Nothing
    <*> areq hiddenField
             (FieldSettings "" Nothing (Just "tz-offset") Nothing [])
             Nothing
    <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

getTodayR :: Handler Html
getTodayR = do
  userId            <- requireAuthId
  user              <- runDB $ get userId
  (widget, enctype) <- generateFormPost dueTimeForm
  let dueTime = case user of
        Just u  -> userDueTime u
        Nothing -> Nothing
  case dueTime of
    Just dt -> do
      currentTime <- liftIO $ utctDayTime <$> getCurrentTime
      let dt' = utctDayTime dt
      let timeOfDay = timeToTimeOfDay $ picosecondsToDiffTime
            (diffTimeToPicoseconds dt' - diffTimeToPicoseconds currentTime)
      let mins = (todHour timeOfDay * 60) + todMin timeOfDay
      tasks' <- runDB $ getTasks userId []
      deps   <- runDB (selectList [] [])
      tasks  <-
        liftIO $ daysList <$> today <*> pure mins <*> pure deps <*> pure tasks'
      defaultLayout $ do
        $(widgetFile "set-time")
        $(widgetFile "tasks")
        toWidget [julius|
          $(function(){
            $('#tz-offset').val(-new Date().getTimezoneOffset());
          });
          |]
    Nothing -> do
      redirect HomeR
      -- defaultLayout $(widgetFile "set-time")

postTodayR :: Handler Html
postTodayR = do
  ((res, widget), enctype) <- runFormPost dueTimeForm
  case res of
    FormSuccess dt -> do
      userId <- requireAuthId

      let userTz = minutesToTimeZone $ tzOffset dt
      utcTime <- liftIO getCurrentTime
      let userTime = utcToLocalTime userTz utcTime

      let (_, utcDueTime) = (localToUTCTimeOfDay userTz $ dueTime dt)

      runDB $ update
        userId
        [UserDueTime =. Just (UTCTime (utctDay utcTime) (timeOfDayToTime utcDueTime))]
      redirect TodayR
    _ -> redirect TodayR

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

daysList
  :: Day -> Int -> [Entity TaskDependency] -> [Entity Task] -> [Entity Task]
daysList day min dependencies =
  sortTasks dependencies . reduceLoad day min . dueByDay day . filter
    (\(Entity _ t) -> not $ taskDone t)

postMarkDoneR :: TaskId -> Handler Html
postMarkDoneR taskId = do
  setUltDestReferer
  runDB $ do
    task <- get taskId
    update taskId [TaskDone =. True]
    deps      <- selectList [TaskDependencyTaskId ==. taskId] []
    newTaskId <- case task of
      Just t -> do
        cdate <- liftIO today
        insert $ incrementDueDate cdate t
      Nothing -> redirectUltDest HomeR
    mapM
      (\(Entity _ dep) ->
        insert $ TaskDependency newTaskId (taskDependencyDependsOnTaskId dep)
      )
      deps
  redirectUltDest HomeR

incrementDueDate :: Day -> Task -> Task
incrementDueDate cdate task = case taskDueDate task of
  Just dd -> case taskRepeat task of
    Just (Days ivl CompletionDate) ->
      task { taskDueDate = (Just (addDays ivl cdate)) }
    Just (Weeks ivl CompletionDate) ->
      task { taskDueDate = (Just (addDays (ivl * 7) cdate)) }
    Just (Months ivl CompletionDate) ->
      task { taskDueDate = (Just (addGregorianMonthsClip ivl cdate)) }
    Just (Years ivl CompletionDate) ->
      task { taskDueDate = (Just (addGregorianYearsClip ivl cdate)) }
    Just (Days ivl DueDate) -> task { taskDueDate = (Just (addDays ivl dd)) }
    Just (Weeks ivl DueDate) ->
      task { taskDueDate = (Just (addDays (ivl * 7) dd)) }
    Just (Months ivl DueDate) ->
      task { taskDueDate = (Just (addGregorianMonthsClip ivl dd)) }
    Just (Years ivl DueDate) ->
      task { taskDueDate = (Just (addGregorianYearsClip ivl dd)) }
    Nothing -> task
  Nothing -> task

getTasks userId = selectList [TaskUserId ==. userId, TaskDone ==. False]
