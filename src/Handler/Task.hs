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

repeatIntervalField :: Field Handler RepeatInterval
repeatIntervalField = Field
  { fieldParse   = \rawVals _ -> case rawVals of
    (i : u : rf : xs)
      | not $ null xs -> return $ Right $ Just $ OnWeekdays
        (map (read . unpack) xs)
        (read $ unpack rf)
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

taskForm :: UserId -> Maybe Task -> Form Task
taskForm userId mtask =
  renderBootstrap3
      (BootstrapHorizontalForm (ColSm 1) (ColSm 3) (ColSm 0) (ColSm 8))
    $   Task
    <$> areq textField "Task Name" (taskName <$> mtask)
    <*> areq intField "Duration in Minutes" (taskDuration <$> mtask)
    <*> areq (selectField optionsEnum) "Priority" (taskPriority <$> mtask)
    <*> aopt (jqueryDayField def { jdsChangeYear = True })
             "Due Date"
             (taskDueDate <$> mtask)
    <*> aopt repeatIntervalField "Repeat Every" (taskRepeat <$> mtask)
    <*> pure False
    <*> pure userId
    <*> pure Nothing
    <*> pure Nothing
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
      (BootstrapHorizontalForm (ColXs 1) (ColXs 3) (ColXs 0) (ColXs 8))
    $   PostponeTodayInfo
    <$> areq timeField "Postpone until later today" Nothing
    <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

postponeDateForm :: Form PostponeDateInfo
postponeDateForm =
  renderBootstrap3
      (BootstrapHorizontalForm (ColXs 1) (ColXs 3) (ColXs 0) (ColXs 8))
    $   PostponeDateInfo
    <$> areq (jqueryDayField def { jdsChangeYear = True })
             "Postpone until future date"
             Nothing
    <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

getNewTaskR :: Handler Html
getNewTaskR = do
  userId                   <- requireAuthId
  ((res, widget), enctype) <- runFormPost $ taskForm userId Nothing
  case res of
    FormSuccess t -> do
      runDB $ insert t
      setMessage "Task created"
      redirectUltDest HomeR
    _ -> do
      defaultLayout $(widgetFile "new-task")

postNewTaskR :: Handler Html
postNewTaskR = getNewTaskR

getEditTaskR :: TaskId -> Handler Html
getEditTaskR taskId = do
  userId                   <- requireAuthId
  task                     <- runDB $ get taskId
  ((res, widget), enctype) <- runFormPost $ taskForm userId $ task
  case res of
    FormSuccess t -> do
      runDB $ replace taskId t
      setMessage "Task updated"
      redirectUltDest HomeR
    _ -> defaultLayout $(widgetFile "edit-task")

postEditTaskR :: TaskId -> Handler Html
postEditTaskR = getEditTaskR

getDeleteTaskR :: TaskId -> Handler ()
getDeleteTaskR taskId = do
  setUltDestReferer
  runDB $ deleteWhere
    (   [TaskDependencyTaskId ==. taskId]
    ||. [TaskDependencyDependsOnTaskId ==. taskId]
    )
  runDB $ delete taskId
  redirectUltDest HomeR

getPostponeTaskR :: TaskId -> Handler Html
getPostponeTaskR taskId = do
  (todayWidget, todayEnctype) <- generateFormPost postponeTodayForm
  (dateWidget , dateEnctype ) <- generateFormPost postponeDateForm
  defaultLayout $(widgetFile "postpone-task")

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
        ]
      redirectUltDest HomeR
    _ -> redirectUltDest HomeR

postPostponeDateR :: TaskId -> Handler ()
postPostponeDateR taskId = do
  ((res, widget), enctype) <- runFormPost postponeDateForm
  case res of
    FormSuccess day -> do
      runDB $ update taskId [TaskPostponeDay =. Just (ppdDay day)]
      redirectUltDest HomeR
    _ -> redirectUltDest HomeR

getUnpostponeR :: TaskId -> Handler ()
getUnpostponeR taskId = do
  runDB
    $ update taskId [TaskPostponeDay =. Nothing, TaskPostponeTime =. Nothing]
  redirectUltDest HomeR
