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

taskList :: [Entity Task] -> Widget
taskList tasks = $(widgetFile "tasks")

getHomeR :: Handler Html
getHomeR = do
  userId <- requireAuthId
  tasks  <- runDB $ selectList [TaskUserId ==. userId] []
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

sortTasks :: [Entity Task] -> [Entity TaskDependency] -> [Entity Task]
sortTasks tasks dependencies =
  concat $ map G.flattenSCC $ sortTasks' tasks dependencies

-- highWeight = 99999
-- mediumWeight = lowWeight * 2
-- lowWeight = 7
-- noneWeight = 0
-- highOverdueMod = 0
-- mediumOverdueMod = lowOverdueMod * 2
-- lowOverdueMod = 1
-- noneOverdueMod = 0
-- weight :: Day -> Task -> Integer
-- weight date task = case _taskPriority task of
--   High -> highWeight + case _taskDueDate task of
--     Just dd -> (date `diffDays` dd) * highOverdueMod
--     Nothing -> 0
--   Medium -> mediumWeight + case _taskDueDate task of
--     Just dd -> (date `diffDays` dd) * mediumOverdueMod
--     Nothing -> 0
--   Low -> lowWeight + case _taskDueDate task of
--     Just dd -> (date `diffDays` dd) * lowOverdueMod
--     Nothing -> 0
--   None -> noneWeight + case _taskDueDate task of
--     Just dd -> (date `diffDays` dd) * noneOverdueMod
--     Nothing -> 0

-- reduceLoad :: Day -> Minutes -> [Task] -> [Task]
-- reduceLoad date min tasks
--   | timeToComplete tasks > min && not (allHighPriority tasks)
--   = reduceLoad date min $ (tail . sortBy lowestWeight) tasks
--   | otherwise
--   = tasks
--  where
--   lowestWeight t1 t2 = weight date t1 `compare` weight date t2
--   allHighPriority t = foldr (\t b -> b && (_taskPriority t) == High) True t

-- timeToComplete :: [Task] -> Minutes
-- timeToComplete = foldr (\t x -> x + (_taskDuration t)) 0

getTodayR :: Handler Html
getTodayR = do
  userId <- requireAuthId
  tasks' <- runDB $ selectList [TaskUserId ==. userId] []
  deps   <- runDB (selectList [] [])
  let tasks = sortTasks tasks' deps
  defaultLayout $ do
    $(widgetFile "tasks")
