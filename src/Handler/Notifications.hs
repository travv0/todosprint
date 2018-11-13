{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Notifications where

import           Import
import           Database.Persist.Sql
import           Text.Read

postUpdateNotifiedR :: Handler Value
postUpdateNotifiedR = do
  mtaskId' <- lookupPostParam "taskId"
  let mtaskId'' = fmap unpack mtaskId'
  let mtaskId = fmap (readMaybe :: String -> Maybe Int64) mtaskId''
  runDB $ case mtaskId of
    Just (Just taskId) -> update (toSqlKey taskId) [TaskNotified =. True]
    _                  -> sendResponseStatus status400 ("bad" :: Text)
  returnJson [("result" :: Text, True)]

getServiceWorkerR :: Handler ()
getServiceWorkerR = do
  app <- getYesod
  let workerPath = unpack (fromMaybe "" (appRoot $ appSettings app)) </> "firebase-messaging-sw.js"
  sendFile "text/javascript" workerPath
