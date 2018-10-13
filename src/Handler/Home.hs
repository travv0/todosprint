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
import           Yesod.Form.Bootstrap3

-- Define our data that will be used for creating the form.
data FileForm = FileForm
  { fileInfo        :: FileInfo
  , fileDescription :: Text
  }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  (formWidget, formEnctype) <- generateFormPost sampleForm
  let submission  = Nothing :: Maybe FileForm
      handlerName = "getHomeR" :: Text
  defaultLayout $ do
    aDomId <- newIdent
    setTitle "Welcome To Yesod!"
    $(widgetFile "homepage")

userForm :: Maybe User -> Form User
userForm muser =
  renderBootstrap3
      (BootstrapHorizontalForm (ColXs 1) (ColXs 2) (ColXs 0) (ColXs 9))
    $   User
    <$> areq textField  "User Name" (userLoginName <$> muser)
    <*> areq emailField "Email"     (userEmail <$> muser)
    <*> aopt textField "First Name" (userFirstName <$> muser)
    <*> aopt textField "Last Name"  (userLastName <$> muser)
    <*> areq passwordField "Password" (userPassword <$> muser)
    <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

getCreateAccountR :: Handler Html
getCreateAccountR = do
  (formWidget, formEnctype) <- generateFormPost $ userForm Nothing
  defaultLayout $ do
    aDomId <- newIdent
    setTitle "Create an Account"
    $(widgetFile "create-account")

postCreateAccountR :: Handler Html
postCreateAccountR = do
  ((result, formWidget), formEnctype) <- runFormPost $ userForm Nothing
  case result of
    FormSuccess user -> do
      u <- runDB $ insert user
      redirect (UserR u)
    _ -> defaultLayout $(widgetFile "create-account")

getUserR :: UserId -> Handler Html
getUserR userId = do
  user  <- runDB $ get404 userId
  tasks <- runDB $ selectList [TaskUserId ==. userId] []
  defaultLayout $ do
    setTitle $ toHtml $ userLoginName user ++ "'s tasks"
    $(widgetFile "tasks")

postHomeR :: Handler Html
postHomeR = do
  ((result, formWidget), formEnctype) <- runFormPost sampleForm
  let handlerName = "postHomeR" :: Text
      submission  = case result of
        FormSuccess res -> Just res
        _               -> Nothing
  defaultLayout $ do
    aDomId <- newIdent
    setTitle "Welcome To Yesod!"
    $(widgetFile "homepage")

sampleForm :: Form FileForm
sampleForm =
  renderBootstrap3 BootstrapBasicForm
    $   FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
 where
  textSettings = FieldSettings
    { fsLabel   = "What's on the aaaaaaaa?"
    , fsTooltip = Nothing
    , fsId      = Nothing
    , fsName    = Nothing
    , fsAttrs = [("class", "form-control"), ("placeholder", "File description")]
    }
