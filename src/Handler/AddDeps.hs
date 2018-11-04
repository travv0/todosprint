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

data TaskDeps = TaskDeps
  { deps :: Maybe [TaskId] }
  deriving Show

depsForm :: [(Text, TaskId)] -> String -> (Maybe [TaskId]) -> Form TaskDeps
depsForm tasks label taskDeps =
  renderBootstrap3
      (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 4))
    $   TaskDeps
    <$> aopt (multiSelectFieldList tasks)
             (bfs ((fromString label) :: Text))
             (Just taskDeps)
    <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

data Dep = Dependency | Dependent
  deriving (Eq, Show)

addDeps :: Dep -> TaskId -> Handler Html
addDeps dep taskId = do
  let depField = case dep of
        Dependency -> TaskDependencyTaskId
        Dependent  -> TaskDependencyDependsOnTaskId
  let optionify (Entity tid task) = (taskName task, tid)
  let getTaskDepId Dependency (Entity _tdid td) =
        taskDependencyDependsOnTaskId td
      getTaskDepId Dependent (Entity _tdid td) = taskDependencyTaskId td
  userId                   <- requireAuthId

  tasks                    <- runDB $ getTasks userId taskId [Asc TaskName]
  taskDeps                 <- runDB $ selectList [depField ==. taskId] []

  ((res, widget), enctype) <-
    runFormPost
    $ depsForm (map optionify tasks)
               (if dep == Dependency then "Dependencies" else "Dependents")
    $ Just
    $ map (getTaskDepId dep) taskDeps

  let msg = case dep of
        Dependency -> "Dependencies updated"
        Dependent  -> "Dependents updated"

  let depsR = case dep of
        Dependency -> AddDepsR
        Dependent  -> AddDependentsR

  mTask <- runDB $ Import.get taskId
  case mTask of
    Just task -> case res of
      FormSuccess ds -> do
        _ <- case deps ds of
          Just jds -> do
            runDB $ do
              deleteWhere [depField ==. taskId]
              case dep of
                Dependency ->
                  mapM (\d -> insert $ TaskDependency taskId d False) jds
                Dependent ->
                  mapM (\d -> insert $ TaskDependency d taskId False) jds
          Nothing -> do
            runDB $ deleteWhere [depField ==. taskId]
            setMessage msg
            redirect $ EditTaskR taskId
        setMessage msg
        redirect $ EditTaskR taskId
      _ -> do
        defaultLayout $ do
          setTitle $ toHtml $ "Add " ++ if dep == Dependency
            then "Dependencies"
            else "Dependents" ++ " for \"" ++ taskName task ++ "\""
          $(widgetFile "add-deps")
    Nothing -> redirectUltDest TodayR

getAddDepsR :: TaskId -> Handler Html
getAddDepsR = addDeps Dependency

postAddDepsR :: TaskId -> Handler Html
postAddDepsR = getAddDepsR

getAddDependentsR :: TaskId -> Handler Html
getAddDependentsR = addDeps Dependent

postAddDependentsR :: TaskId -> Handler Html
postAddDependentsR = getAddDependentsR

getTasks
  :: (PersistQueryRead backend, MonadIO m, BaseBackend backend ~ SqlBackend)
  => Key User
  -> Key Task
  -> [SelectOpt Task]
  -> ReaderT backend m [Entity Task]
getTasks userId taskId = selectList
  [ TaskUserId ==. userId
  , TaskId !=. taskId
  , TaskDone ==. False
  , TaskDeleted ==. False
  ]
