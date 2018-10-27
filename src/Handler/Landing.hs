{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Landing where

import           Import
import           Yesod.Auth.GoogleEmail2

getLandingR :: Handler Html
getLandingR = do
  userId <- maybeAuthId
  case userId of
    Just _  -> redirect TodayR
    Nothing -> defaultLayout $(widgetFile "landing")
