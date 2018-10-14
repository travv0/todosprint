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

taskList :: [Entity Task] -> Widget
taskList tasks = $(widgetFile "tasks")

getHomeR :: Handler Html
getHomeR = do
  userId <- requireAuthId
  tasks  <- runDB $ selectList [TaskUserId ==. userId] [Asc TaskDueDate]
  defaultLayout $ do
    setTitle "Your Tasks"
    $(widgetFile "homepage")

getUserR :: UserId -> Handler Html
getUserR userId = do
  user  <- runDB $ get404 userId
  tasks <- runDB $ selectList [TaskUserId ==. userId] []
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
    (date `diffDays` dd)
      * modifier
      - (toInteger (taskDuration task) `div` (modifier * (date `diffDays` dd)))

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
  tasks' <- runDB $ selectList [TaskUserId ==. userId] []
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
  tasks' <- runDB $ selectList [TaskUserId ==. userId] []
  deps   <- runDB (selectList [] [])
  tasks  <-
    liftIO $ daysList <$> today <*> pure min <*> pure deps <*> pure tasks'

  defaultLayout $ do
    $(widgetFile "tasks")
