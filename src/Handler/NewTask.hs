{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.NewTask where

import           Database.Persist.Sql
import           Yesod.Form.Bootstrap3
import           Import
import           Yesod.Form.Jquery
import           Priority
import           RepeatInterval
import           Text.Read

repeatIntervalField :: Field Handler RepeatInterval
repeatIntervalField = Field
  { fieldParse   = \rawVals _ -> case rawVals of
    [i, u, rf]
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
  , fieldView    = \idAttr nameAttr otherAttrs eResult isReq ->
    $(widgetFile "repeat-interval")
  , fieldEnctype = UrlEncoded
  }

taskForm :: UserId -> Maybe Task -> Form Task
taskForm userId mtask =
  renderBootstrap3
      (BootstrapHorizontalForm (ColXs 1) (ColXs 3) (ColXs 0) (ColXs 8))
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
    <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

getNewTaskR :: Handler Html
getNewTaskR = do
  userId            <- requireAuthId
  (widget, enctype) <- generateFormPost $ taskForm userId Nothing
  defaultLayout $(widgetFile "new-task")

postNewTaskR :: Handler Html
postNewTaskR = do
  userId                   <- requireAuthId
  ((res, widget), enctype) <- runFormPost $ taskForm userId Nothing
  case res of
    FormSuccess t -> do
      runDB $ insert t
      redirect HomeR
    _ -> defaultLayout $(widgetFile "new-task")
