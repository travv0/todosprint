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
      (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 4))
    $   TaskDeps
    <$> aopt (multiSelectFieldList tasks)
             (bfs ((fromString label) :: Text))
             (Just taskDeps)
    <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

getAddDepsR :: TaskId -> Handler Html
getAddDepsR taskId = do
  let optionify (Entity taskId task) = (taskName task, taskId)
  let getTaskDepId (Entity tdid td) = taskDependencyDependsOnTaskId td
  userId                   <- requireAuthId

  tasks                    <- runDB $ getTasks userId taskId [Asc TaskName]
  taskDeps <- runDB $ selectList [TaskDependencyTaskId ==. taskId] []

  ((res, widget), enctype) <-
    runFormPost
    $ depsForm taskId (map optionify tasks) "Dependencies"
    $ Just
    $ map getTaskDepId taskDeps

  mTask <- runDB $ Import.get taskId
  case mTask of
    Just task -> case res of
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
        defaultLayout $ do
          setTitle $ toHtml $ "Add Dependencies for \"" ++ taskName task ++ "\""
          [whamlet|<h3>Add Dependencies for "#{taskName task}"|]
          $(widgetFile "add-deps")
    Nothing -> redirectUltDest TodayR

postAddDepsR :: TaskId -> Handler Html
postAddDepsR = getAddDepsR

getAddDependentsR :: TaskId -> Handler Html
getAddDependentsR taskId = do
  let optionify (Entity taskId task) = (taskName task, taskId)
  let getTaskDepId (Entity tdid td) = taskDependencyTaskId td
  userId                   <- requireAuthId

  tasks                    <- runDB $ getTasks userId taskId [Asc TaskName]
  taskDeps <- runDB $ selectList [TaskDependencyDependsOnTaskId ==. taskId] []

  ((res, widget), enctype) <-
    runFormPost
    $ depsForm taskId (map optionify tasks) "Dependents"
    $ Just
    $ map getTaskDepId taskDeps

  mTask <- runDB $ Import.get taskId
  case mTask of
    Just task -> case res of
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
        defaultLayout $ do
          setTitle $ toHtml $ "Add Dependents for \"" ++ taskName task ++ "\""
          [whamlet|<h3>Add Dependents for "#{taskName task}"|]
          $(widgetFile "add-dependents")
    Nothing -> redirectUltDest TodayR

postAddDependentsR :: TaskId -> Handler Html
postAddDependentsR = getAddDependentsR

getTasks userId taskId = selectList
  [ TaskUserId ==. userId
  , TaskId !=. taskId
  , TaskDone ==. False
  , TaskDeleted ==. False
  ]
