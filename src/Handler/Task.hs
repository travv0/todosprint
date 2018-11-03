{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Task where

import           Database.Persist.Sql
import           Yesod.Form.Bootstrap3
import           Import
import           Yesod.Form.Jquery
import           RepeatInterval
import           Text.Read                      ( read
                                                , readMaybe
                                                )
import           Data.Time.LocalTime
import           Data.Time
import qualified Data.List                     as L
import           Handler.Home

repeatIntervalField :: Field Handler RepeatInterval
repeatIntervalField = Field
  { fieldParse   = \rawVals _ -> case rawVals of
    (rt : i : u : rf : xs)
      | rt == "No" -> return $ Right Nothing
      | rt == "DayOfWeek" && not (null xs) -> return $ Right $ Just $ OnWeekdays
        (map (read . unpack) xs)
        CompletionDate
      | rt == "DayOfWeek" -> return $ Right Nothing
      | isNothing ((readMaybe $ unpack i) :: Maybe Int) -> return
      $  Right Nothing
      | u == "Days" -> return $ Right $ Just $ Days (read $ unpack i)
                                                    (read $ unpack rf)
      | u == "Weeks" -> return $ Right $ Just $ Weeks (read $ unpack i)
                                                      (read $ unpack rf)
      | u == "Months" -> return $ Right $ Just $ Months (read $ unpack i)
                                                        (read $ unpack rf)
      | u == "Years" -> return $ Right $ Just $ Years (read $ unpack i)
                                                      (read $ unpack rf)
      | otherwise -> return $ Left "Invalid unit of time for repeat interval"
    _ -> return $ Right Nothing
  , fieldView    = \idAttr nameAttr otherAttrs eResult isReq -> case eResult of
    Right (Days n ri) -> do
      let days = []
      let u    = "Days"
      $(widgetFile "repeat-interval")
    Right (Weeks n ri) -> do
      let days = []
      let u    = "Weeks"
      $(widgetFile "repeat-interval")
    Right (Months n ri) -> do
      let days = []
      let u    = "Months"
      $(widgetFile "repeat-interval")
    Right (Years n ri) -> do
      let days = []
      let u    = "Years"
      $(widgetFile "repeat-interval")
    Right (OnWeekdays days ri) -> do
      let n = 0
      let u = ""
      $(widgetFile "repeat-interval")
    Left _ -> do
      let (n, ri, days, u) = (0, CompletionDate, [], "")
      $(widgetFile "repeat-interval")
  , fieldEnctype = UrlEncoded
  }

startTimeField :: Field Handler UTCTime
startTimeField = Field
  { fieldParse   = \rawVals _ -> do
    case rawVals of
      []   -> return $ Right Nothing
      [""] -> return $ Right Nothing
      _    -> do
        (Entity _ user) <- requireAuth
        let mUserTime =
              parseTimeM True defaultTimeLocale "%R" (unpack $ L.head rawVals) :: Maybe
                  TimeOfDay
        case mUserTime of
          Nothing       -> return $ Left "No start time"
          Just userTime -> do
            let userTz            = fromMaybe utc $ userTimeZone user
            let (dayAdj, utcTime) = localToUTCTimeOfDay userTz userTime
            utcDateTime <- liftIO getCurrentTime
            let userDateTime = utcToLocalTime userTz utcDateTime
            return $ Right $ Just
              (UTCTime (addDays dayAdj (localDay userDateTime))
                       (timeOfDayToTime utcTime)
              )
  , fieldView    = \idAttr nameAttr otherAttrs eResult isReq -> case eResult of
    Right startTime -> do
      (Entity _ user) <- requireAuth
      let userTz        = fromMaybe utc $ userTimeZone user
      let userTime      = localTimeOfDay $ utcToLocalTime userTz startTime
      let formattedTime = formatTime defaultTimeLocale "%R" userTime
      [whamlet|<input type="time" id=#{idAttr} name=#{nameAttr} *{otherAttrs} :isReq:required value="#{formattedTime}">|]
    Left _ -> do
      [whamlet|<input type="time" id=#{idAttr} name=#{nameAttr} *{otherAttrs} :isReq:required>|]
  , fieldEnctype = UrlEncoded
  }

taskForm :: UserId -> UTCTime -> Maybe Task -> Form Task
taskForm userId currUtcTime mtask =
  renderBootstrap3
      (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 4))
    $   Task
    <$> areq textField (bfs ("Task Name" :: Text)) (taskName <$> mtask)
    <*> areq intField
             (bfs ("Duration in Minutes" :: Text))
             (taskDuration <$> mtask)
    <*> areq (selectField optionsEnum)
             (bfs ("Priority" :: Text))
             (taskPriority <$> mtask)
    <*> aopt (jqueryDayField def { jdsChangeYear = True })
             (bfs ("Due Date" :: Text))
             (taskDueDate <$> mtask)
    <*> aopt
          startTimeField
          (bfs ("Start Time" :: Text))
          (case mtask of
            Just task -> if taskPostponeTimeRepeat task
              then Just (taskPostponeTime task)
              else Nothing
            Nothing -> Nothing
          )
    <*> aopt repeatIntervalField (bfs ("Repeat" :: Text)) (taskRepeat <$> mtask)
    <*> pure False
    <*> pure userId
    <*> pure Nothing
    <*> pure currUtcTime
    <*> pure Nothing
    <*> pure False
    <*> pure Nothing
    <*> pure True
    <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

data PostponeTodayInfo = PostponeTodayInfo
  { pptTime :: TimeOfDay }
  deriving Show

data PostponeDateInfo = PostponeDateInfo
  { ppdDay :: Day }
  deriving Show

postponeTodayForm :: Form PostponeTodayInfo
postponeTodayForm =
  renderBootstrap3
      (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 4))
    $   PostponeTodayInfo
    <$> areq timeField (bfs ("Postpone until later today" :: Text)) Nothing
    <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

postponeDateForm :: Form PostponeDateInfo
postponeDateForm =
  renderBootstrap3
      (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 4))
    $   PostponeDateInfo
    <$> areq (jqueryDayField def { jdsChangeYear = True })
             (bfs ("Postpone until future date" :: Text))
             Nothing
    <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

getNewTaskR :: Handler Html
getNewTaskR = do
  userId                   <- requireAuthId
  currTime                 <- liftIO getCurrentTime
  ((res, widget), enctype) <- runFormPost $ taskForm userId currTime Nothing
  case res of
    FormSuccess t -> do
      runDB $ insert t
        { taskPostponeTimeRepeat = if isJust (taskPostponeTime t)
                                     then True
                                     else False
        }
      setMessage "Task created"
      redirect NewTaskR
    _ -> do
      defaultLayout $ do
        setTitle "New Task"
        $(widgetFile "new-task")

postNewTaskR :: Handler Html
postNewTaskR = getNewTaskR

getEditTaskR :: TaskId -> Handler Html
getEditTaskR taskId = do
  userId                   <- requireAuthId
  task                     <- runDB $ get taskId
  currTime                 <- liftIO getCurrentTime
  ((res, widget), enctype) <- runFormPost $ taskForm userId currTime $ task
  case res of
    FormSuccess t -> do
      let repeatTime = if isJust (taskPostponeTime t) then True else False
      case task of
        Just t2 -> runDB $ replace taskId $ t
          { taskPostponeTimeRepeat = repeatTime
          , taskPostponeTime       = if not repeatTime
                                        && isJust (taskPostponeTime t2)
                                        && not (taskPostponeTimeRepeat t2)
                                     then
                                       taskPostponeTime t2
                                     else
                                       taskPostponeTime t
          , taskPostponeDay        = taskPostponeDay t2
          }
        Nothing -> runDB $ replace taskId t
      setMessage "Task updated"
      redirectUltDest HomeR
    _ -> defaultLayout $ do
      setTitle "Edit Task"
      $(widgetFile "edit-task")

postEditTaskR :: TaskId -> Handler Html
postEditTaskR = getEditTaskR

getDeleteTaskR :: TaskId -> Handler ()
getDeleteTaskR taskId = do
  setUltDestReferer
  currTime <- liftIO getCurrentTime
  runDB $ updateWhere
    (   [TaskDependencyTaskId ==. taskId]
    ||. [TaskDependencyDependsOnTaskId ==. taskId]
    )
    [TaskDependencyDeleted =. True]
  runDB $ update taskId [TaskDeleted =. True, TaskDeleteTime =. Just currTime]
  redirectUltDest HomeR

getPostponeTaskR :: TaskId -> Handler Html
getPostponeTaskR taskId = do
  (todayWidget, todayEnctype) <- generateFormPost postponeTodayForm
  (dateWidget , dateEnctype ) <- generateFormPost postponeDateForm
  mTask                       <- runDB $ get taskId
  case mTask of
    Just task -> defaultLayout $ do
      setTitle $ toHtml $ "Postpone \"" ++ taskName task ++ "\""
      [whamlet|<h3>Postpone "#{taskName task}"|]
      $(widgetFile "postpone-task")
    Nothing -> redirectUltDest TodayR

postPostponeTodayR :: TaskId -> Handler ()
postPostponeTodayR taskId = do
  (Entity _ user)          <- requireAuth
  ((res, widget), enctype) <- runFormPost postponeTodayForm
  case res of
    FormSuccess t -> do
      let ppTime = pptTime t
      currUtcTime <- liftIO getCurrentTime
      let tz = fromMaybe utc $ minutesToTimeZone <$> userDueTimeOffset user
      let (dayAdj, utcTime) = localToUTCTimeOfDay tz ppTime
      let userTime = utcToLocalTime tz currUtcTime
      runDB $ update
        taskId
        [ TaskPostponeTime
          =. Just
               (UTCTime (addDays dayAdj (localDay userTime))
                        (timeOfDayToTime utcTime)
               )
        , TaskPostponeDay =. Nothing
        ]
      redirectUltDest HomeR
    _ -> redirectUltDest HomeR

postPostponeDateR :: TaskId -> Handler ()
postPostponeDateR taskId = do
  ((res, widget), enctype) <- runFormPost postponeDateForm
  case res of
    FormSuccess day -> do
      runDB $ update
        taskId
        [TaskPostponeDay =. Just (ppdDay day), TaskPostponeTime =. Nothing]
      redirectUltDest HomeR
    _ -> redirectUltDest HomeR

getUnpostponeR :: TaskId -> Handler ()
getUnpostponeR taskId = do
  runDB
    $ update taskId [TaskPostponeDay =. Nothing, TaskPostponeTime =. Nothing]
  redirectUltDest HomeR
