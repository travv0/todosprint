{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.AddDeps where

import           Database.Persist.Sql
import           Yesod.Form.Bootstrap3
import           Import
import           Yesod.Form.Jquery
import           Priority
import           RepeatInterval
import           Text.Read

data TaskDeps = TaskDeps
  { deps :: Maybe [TaskId] }
  deriving Show

depsForm
  :: TaskId -> [(Text, TaskId)] -> String -> (Maybe [TaskId]) -> Form TaskDeps
depsForm taskId tasks label taskDeps =
  renderBootstrap3
      (BootstrapHorizontalForm (ColXs 0) (ColXs 4) (ColXs 0) (ColXs 8))
    $   TaskDeps
    <$> aopt (checkboxesFieldList tasks)
             (bfs ((fromString label) :: Text))
             (Just taskDeps)
    <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

getAddDepsR :: TaskId -> Handler Html
getAddDepsR taskId = do
  let optionify (Entity taskId task) = (taskName task, taskId)
  let getTaskDepId (Entity tdid td) = taskDependencyDependsOnTaskId td
  userId                   <- requireAuthId

  tasks                    <- runDB $ getTasks userId taskId
  taskDeps <- runDB $ selectList [TaskDependencyTaskId ==. taskId] []

  ((res, widget), enctype) <-
    runFormPost
    $ depsForm taskId (map optionify tasks) "Dependencies"
    $ Just
    $ map getTaskDepId taskDeps

  case res of
    FormSuccess ds -> do
      case deps ds of
        Just jds -> do
          runDB $ do
            deleteWhere [TaskDependencyTaskId ==. taskId]
            mapM (\d -> insert $ TaskDependency taskId d False) jds
        Nothing -> do
          runDB $ deleteWhere [TaskDependencyTaskId ==. taskId]
          setMessage "Dependencies updated"
          redirect $ EditTaskR taskId
      setMessage "Dependencies updated"
      redirect $ EditTaskR taskId
    _ -> do
      defaultLayout $(widgetFile "add-deps")

postAddDepsR :: TaskId -> Handler Html
postAddDepsR = getAddDepsR

getAddDependentsR :: TaskId -> Handler Html
getAddDependentsR taskId = do
  let optionify (Entity taskId task) = (taskName task, taskId)
  let getTaskDepId (Entity tdid td) = taskDependencyTaskId td
  userId                   <- requireAuthId

  tasks                    <- runDB $ getTasks userId taskId
  taskDeps <- runDB $ selectList [TaskDependencyDependsOnTaskId ==. taskId] []

  ((res, widget), enctype) <-
    runFormPost
    $ depsForm taskId (map optionify tasks) "Dependents"
    $ Just
    $ map getTaskDepId taskDeps

  case res of
    FormSuccess ds -> do
      case deps ds of
        Just jds -> do
          runDB $ do
            deleteWhere [TaskDependencyDependsOnTaskId ==. taskId]
            mapM (\d -> insert $ TaskDependency d taskId False) jds
        Nothing -> do
          runDB $ deleteWhere [TaskDependencyDependsOnTaskId ==. taskId]
          setMessage "Dependents updated"
          redirect $ EditTaskR taskId
      setMessage "Dependents updated"
      redirect $ EditTaskR taskId
    _ -> do
      defaultLayout $(widgetFile "add-dependents")

postAddDependentsR :: TaskId -> Handler Html
postAddDependentsR = getAddDependentsR

getTasks userId taskId = selectList
  [ TaskUserId ==. userId
  , TaskId !=. taskId
  , TaskDone ==. False
  , TaskDeleted ==. False
  ]
  []
