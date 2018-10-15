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
import qualified Data.Time.Units               as TU
import qualified Data.List                     as L
import           RepeatInterval
import           Yesod.Form.Bootstrap3
import           Data.Maybe
import           Text.Julius                    ( RawJS(..) )

taskList :: [Entity Task] -> Widget
taskList tasks = $(widgetFile "tasks")

getHomeR :: Handler Html
getHomeR = do
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
 where
  weightMod dd modifier =
    let divisor = modifier
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

dueTimeForm :: Text -> Maybe TimeOfDay -> Form DueTime
dueTimeForm tzOffsetId mdt =
  renderBootstrap3
      (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 8))
    $   DueTime
    <$> areq timeField "When would you like to work until?" mdt
    <*> areq hiddenField
             (FieldSettings "" Nothing (Just tzOffsetId) Nothing [])
             Nothing
    <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

getTodayR :: Handler Html
getTodayR = do
  (Entity userId user) <- requireAuth
  let mUserDueTime = userDueTime user
  tzOffsetId <- newIdent
  case mUserDueTime of
    Just dt -> do
      let userTz = maybe utc minutesToTimeZone (userDueTimeOffset user)

      let mLocalTod = fmap
            (utcToLocalTimeOfDay userTz . timeToTimeOfDay . utctDayTime)
            mUserDueTime
      (widget, enctype) <- generateFormPost $ dueTimeForm tzOffsetId (snd <$> mLocalTod)

      currentTime       <- liftIO $ utctDayTime <$> getCurrentTime
      let currentTime' = timeToTimeOfDay currentTime
      let (_, currentTime'') = utcToLocalTimeOfDay userTz currentTime'
      let curTime = timeOfDayToTime currentTime''

      let dt' = timeToTimeOfDay $ utctDayTime dt
      let (_, dt'') = utcToLocalTimeOfDay userTz dt'
      let dTime = timeOfDayToTime dt''

      let timeOfDay = timeToTimeOfDay $ picosecondsToDiffTime
            (diffTimeToPicoseconds dTime - diffTimeToPicoseconds curTime)
      let mins = (todHour timeOfDay * 60) + todMin timeOfDay

      tasks' <- runDB $ getTasks userId []
      deps   <- runDB (selectList [] [])

      utcTime <- liftIO getCurrentTime
      let today = localDay $ utcToLocalTime userTz utcTime
      let tasks = daysList today mins deps tasks'

      defaultLayout $ do
        setTimeWidget widget enctype tzOffsetId
        $(widgetFile "tasks")
    Nothing -> do
      (widget, enctype) <- generateFormPost $ dueTimeForm tzOffsetId Nothing
      defaultLayout $ setTimeWidget widget enctype tzOffsetId

setTimeWidget :: Widget -> Enctype -> Text -> Widget
setTimeWidget widget enctype tzOffsetId = do
  $(widgetFile "set-time")
  toWidget [julius|
    $(function(){
          $(#{rawJS tzOffsetId}).val(-new Date().getTimezoneOffset());
    });
          |]

postTodayR :: Handler Html
postTodayR = do
  tzOffsetId               <- newIdent
  ((res, widget), enctype) <- runFormPost $ dueTimeForm tzOffsetId Nothing
  case res of
    FormSuccess dt -> do
      userId <- requireAuthId

      let userTz = minutesToTimeZone $ tzOffset dt
      utcTime <- liftIO getCurrentTime
      let userTime        = utcToLocalTime userTz utcTime

      let (_, utcDueTime) = localToUTCTimeOfDay userTz $ dueTime dt

      runDB $ update
        userId
        [ UserDueTime
          =. Just (UTCTime (utctDay utcTime) (timeOfDayToTime utcDueTime))
        , UserDueTimeOffset =. Just (tzOffset dt)
        ]
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
  task <- runDB $ get taskId
  runDB $ update taskId [TaskDone =. True]
  deps  <- runDB $ selectList [TaskDependencyTaskId ==. taskId] []
  utcTime <- liftIO getCurrentTime
  (Entity userId user) <- requireAuth
  let userTzOffset = fromMaybe 0 $ userDueTimeOffset user
  let cdate = localDay $ utcToLocalTime (minutesToTimeZone userTzOffset) utcTime
  runDB $ case task of
    Just t -> case incrementDueDate cdate t of
      Just t2 -> do
        newTaskId <- insert t2
        mapM
          (\(Entity _ dep) -> insert
            $ TaskDependency newTaskId (taskDependencyDependsOnTaskId dep)
          )
          deps
      Nothing -> redirectUltDest HomeR
    Nothing -> redirectUltDest HomeR
  redirectUltDest HomeR

incrementDueDate :: Day -> Task -> Maybe Task
incrementDueDate cdate task = case taskDueDate task of
  Just dd -> case taskRepeat task of
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
