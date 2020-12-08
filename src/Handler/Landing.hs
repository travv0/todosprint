{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Landing where

import Import
import Yesod.Auth.OAuth2 (oauth2Url)

getLandingR :: Handler Html
getLandingR = do
  userId <- maybeAuthId
  case userId of
    Just _ -> redirect TodayR
    Nothing -> defaultLayout $(widgetFile "landing")
