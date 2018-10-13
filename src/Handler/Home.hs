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

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  userId <- requireAuthId
  tasks  <- runDB $ do
    selectList [TaskUserId ==. userId] []
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
