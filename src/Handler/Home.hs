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
import qualified Data.List                     as L
import           RepeatInterval

taskList :: [Entity Task] -> Widget
taskList tasks = $(widgetFile "tasks")

getHomeR :: Handler Html
getHomeR = do
  userId <- requireAuthId
  tasks  <- runDB $ getTasks userId [Desc TaskId]
  defaultLayout $ do
    setTitle "Your Tasks"
    $(widgetFile "homepage")

getUserR :: UserId -> Handler Html
getUserR userId = do
  user  <- runDB $ get404 userId
  tasks <- runDB $ getTasks userId []
  defaultLayout $ do
    setTitle $ toHtml $ userEmail user ++ "'s tasks"
    $(widgetFile "tasks")

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

getTodayR :: Handler Html
getTodayR = do
  userId <- requireAuthId
  tasks' <- runDB $ getTasks userId []
  deps   <- runDB (selectList [] [])
  let tasks = sortTasks deps tasks'
  defaultLayout $ do
    $(widgetFile "tasks")

dueByDay :: Day -> [Entity Task] -> [Entity Task]
dueByDay day = filter (dueByOrBeforeDay day)
 where
  dueByOrBeforeDay d (Entity _ t) = case taskDueDate t of
    Just dd -> dd <= d
    Nothing -> True

today :: IO Day
today = localDay . zonedTimeToLocalTime <$> getZonedTime

daysList
  :: Day -> Int -> [Entity TaskDependency] -> [Entity Task] -> [Entity Task]
daysList day min dependencies =
  sortTasks dependencies . reduceLoad day min . dueByDay day . filter
    (\(Entity _ t) -> not $ taskDone t)

getTodayMinR :: Int -> Handler Html
getTodayMinR min = do
  userId <- requireAuthId
  tasks' <- runDB $ getTasks userId []
  deps   <- runDB (selectList [] [])
  tasks  <-
    liftIO $ daysList <$> today <*> pure min <*> pure deps <*> pure tasks'

  defaultLayout $ do
    $(widgetFile "tasks")

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
