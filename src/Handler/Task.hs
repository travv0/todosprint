{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- HLINT "Redundant if" -}

module Handler.Task where

import Common
import qualified Data.List as L
import Data.Time (addDays)
import Data.Time.LocalTime
import Database.Persist.Sql
import Import
import RepeatInterval
import Text.Read (read, readMaybe)
import Yesod.Form.Bootstrap3
import Yesod.Form.Jquery

repeatIntervalField :: Field Handler RepeatInterval
repeatIntervalField =
    Field
        { fieldParse = \rawVals _ -> case rawVals of
            (rt : i : u : rf : xs)
                | rt == "No" -> return $ Right Nothing
                | rt == "DayOfWeek" && not (null xs) ->
                    return $ Right $ Just $ OnWeekdays (map (read . unpack) xs)
                | rt == "DayOfWeek" -> return $ Right Nothing
                | isNothing ((readMaybe $ unpack i) :: Maybe Int) ->
                    return $ Right Nothing
                | Just unit <- (readMaybe (unpack u) :: Maybe UnitOfTime) ->
                    return $ Right $ Just $ ByUnitOfTime unit (read $ unpack i) (read $ unpack rf)
                | otherwise -> return $ Left "Invalid unit of time for repeat interval"
            _ -> return $ Right Nothing
        , fieldView = \idAttr nameAttr otherAttrs eResult isReq -> case eResult of
            Right (ByUnitOfTime unit n ri) -> do
                let days = []
                    u = unit
                $(widgetFile "repeat-interval")
            Right (OnWeekdays days) -> do
                let n = 0 :: Integer
                    u = Days
                    ri = CompletionDate
                $(widgetFile "repeat-interval")
            Left _ -> do
                let (n, ri, days, u) = (0 :: Integer, CompletionDate, [], Days)
                $(widgetFile "repeat-interval")
        , fieldEnctype = UrlEncoded
        }

startTimeField :: Field Handler UTCTime
startTimeField =
    Field
        { fieldParse = \rawVals _ ->
            case rawVals of
                [] -> return $ Right Nothing
                [""] -> return $ Right Nothing
                _ -> do
                    (Entity _ user) <- requireAuth
                    let mUserTime =
                            parseTimeM True defaultTimeLocale "%R" (unpack $ L.head rawVals) ::
                                Maybe
                                    TimeOfDay
                    case mUserTime of
                        Nothing -> return $ Left "No start time"
                        Just userTime -> do
                            let userTz = fromMaybe utc $ userTimeZone user
                            let (dayAdj, utcTime) = localToUTCTimeOfDay userTz userTime
                            utcDateTime <- liftIO getCurrentTime
                            let userDateTime = utcToLocalTime userTz utcDateTime
                            return $
                                Right $
                                    Just
                                        ( UTCTime
                                            (addDays dayAdj (localDay userDateTime))
                                            (timeOfDayToTime utcTime)
                                        )
        , fieldView = \idAttr nameAttr otherAttrs eResult isReq -> case eResult of
            Right startTime -> do
                (Entity _ user) <- requireAuth
                let userTz = fromMaybe utc $ userTimeZone user
                let userTime = localTimeOfDay $ utcToLocalTime userTz startTime
                let formattedTime = formatTime defaultTimeLocale "%R" userTime
                [whamlet|<input type="time" id=#{idAttr} name=#{nameAttr} *{otherAttrs} :isReq:required value="#{formattedTime}">|]
            Left _ ->
                [whamlet|<input type="time" id=#{idAttr} name=#{nameAttr} *{otherAttrs} :isReq:required>|]
        , fieldEnctype = UrlEncoded
        }

taskForm :: UserId -> UTCTime -> Maybe Task -> Form Task
taskForm userId currUtcTime mtask =
    renderBootstrap3
        (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 4))
        $ Task
            <$> areq textField (bfs ("Task Name" :: Text)) (taskName <$> mtask)
            <*> areq
                intField
                (bfs ("Duration in Minutes" :: Text))
                (taskDuration <$> mtask)
            <*> areq
                (selectField optionsEnum)
                (bfs ("Priority" :: Text))
                (taskPriority <$> mtask)
            <*> aopt
                (jqueryDayField def{jdsChangeYear = True})
                (bfs ("Due Date" :: Text))
                (taskDueDate <$> mtask)
            <*> aopt repeatIntervalField (bfs ("Repeat" :: Text)) (taskRepeat <$> mtask)
            <*> pure False
            <*> pure userId
            <*> pure Nothing
            <*> pure currUtcTime
            <*> pure Nothing
            <*> pure False
            <*> pure Nothing
            <*> pure False
            <* bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

newtype PostponeTodayInfo = PostponeTodayInfo
    {pptTime :: TimeOfDay}
    deriving (Show)

newtype PostponeDateInfo = PostponeDateInfo
    {ppdDay :: Day}
    deriving (Show)

postponeTodayForm :: Form PostponeTodayInfo
postponeTodayForm =
    renderBootstrap3
        (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 4))
        $ PostponeTodayInfo
            <$> areq timeField (bfs ("Postpone until later today" :: Text)) Nothing
            <* bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

postponeDateForm :: Form PostponeDateInfo
postponeDateForm =
    renderBootstrap3
        (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 4))
        $ PostponeDateInfo
            <$> areq
                (jqueryDayField def{jdsChangeYear = True})
                (bfs ("Postpone until future date" :: Text))
                Nothing
            <* bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

getNewTaskR :: Handler Html
getNewTaskR = do
    Entity userId _user <- requireAuth
    currTime <- liftIO getCurrentTime
    ((res, widget), enctype) <- runFormPost $ taskForm userId currTime Nothing
    case res of
        FormSuccess t -> do
            _ <- runDB $ insert t
            setMessage "Task created"
            redirect NewTaskR
        _ ->
            defaultLayout $ do
                setTitle "New Task"
                $(widgetFile "new-task")

postNewTaskR :: Handler Html
postNewTaskR = getNewTaskR

getEditTaskR :: TaskId -> Handler Html
getEditTaskR taskId = do
    Entity userId _ <- requireAuth
    mtask <- runDB $ get taskId
    currTime <- liftIO getCurrentTime
    ((res, widget), enctype) <- runFormPost $ taskForm userId currTime mtask
    case res of
        FormSuccess newTask -> do
            case mtask of
                Just oldTask ->
                    runDB $
                        replace taskId $
                            newTask
                                { taskPostponeDay = taskPostponeDay oldTask
                                , taskPinned = taskPinned oldTask
                                }
                Nothing -> runDB $ replace taskId newTask
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
    runDB $ update taskId [TaskDeleted =. True, TaskDeleteTime =. Just currTime]
    redirectUltDest HomeR

getPinTaskR :: TaskId -> Handler ()
getPinTaskR taskId = do
    setUltDestReferer
    mTask <- runDB $ get taskId
    case mTask of
        Just task ->
            runDB $ update taskId [TaskPinned =. not (taskPinned task)]
        Nothing -> pure ()
    redirectUltDest HomeR

getPostponeTaskR :: TaskId -> Handler Html
getPostponeTaskR taskId = do
    (dateWidget, dateEnctype) <- generateFormPost postponeDateForm
    mTask <- runDB $ get taskId
    case mTask of
        Just task -> defaultLayout $ do
            setTitle $ toHtml $ "Postpone \"" ++ taskName task ++ "\""
            [whamlet|<h3>Postpone "#{taskName task}"|]
            $(widgetFile "postpone-task")
        Nothing -> redirectUltDest TodayR

postPostponeDateR :: TaskId -> Handler ()
postPostponeDateR taskId = do
    ((res, _widget), _enctype) <- runFormPost postponeDateForm
    case res of
        FormSuccess day -> do
            runDB $
                update
                    taskId
                    [ TaskPostponeDay =. Just (ppdDay day)
                    , TaskPinned =. False
                    ]
            redirectUltDest HomeR
        _ -> redirectUltDest HomeR

getUnpostponeR :: TaskId -> Handler ()
getUnpostponeR taskId = do
    runDB $ update taskId [TaskPostponeDay =. Nothing]
    redirectUltDest HomeR
