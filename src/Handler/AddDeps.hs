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

depsForm :: TaskId -> [(Text, TaskId)] -> (Maybe [TaskId]) -> Form TaskDeps
depsForm taskId tasks taskDeps =
  renderBootstrap3
      (BootstrapHorizontalForm (ColXs 1) (ColXs 3) (ColXs 0) (ColXs 8))
    $   TaskDeps
    <$> aopt (checkboxesFieldList tasks) "Dependencies" (Just taskDeps)
    <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

getAddDepsR :: TaskId -> Handler Html
getAddDepsR taskId = do
  let optionify (Entity taskId task) = (taskName task, taskId)
  let getTaskDepId (Entity tdid td) = taskDependencyDependsOnTaskId td
  userId <- requireAuthId

  tasks  <- runDB $ selectList
    [TaskUserId ==. userId, TaskId !=. taskId, TaskDone ==. False]
    []
  taskDeps <- runDB $ selectList [TaskDependencyTaskId ==. taskId] []

  ((res, widget), enctype) <-
    runFormPost $ depsForm taskId (map optionify tasks) $ Just $ map
      getTaskDepId
      taskDeps

  case res of
    FormSuccess ds -> do
      case deps ds of
        Just jds -> do
          runDB $ do
            deleteWhere [TaskDependencyTaskId ==. taskId]
            mapM (\d -> insert $ TaskDependency taskId d) jds
        Nothing -> do
          runDB $ deleteWhere [TaskDependencyTaskId ==. taskId]
          setMessage "Dependencies updated"
          redirectUltDest HomeR
      setMessage "Dependencies updated"
      redirectUltDest HomeR
    _ -> do
      defaultLayout $(widgetFile "add-deps")

postAddDepsR :: TaskId -> Handler Html
postAddDepsR = getAddDepsR
