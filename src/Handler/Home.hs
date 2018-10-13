{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Database.Persist.Sql
import           Import
import           Text.Julius                    ( RawJS(..) )

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
