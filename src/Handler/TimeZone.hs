{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.TimeZone where

import           Import
import           Database.Persist.Sql
import           Text.Read

getTimeZoneR :: Handler Html
getTimeZoneR = redirect TodayR

postTimeZoneR :: Handler Value
postTimeZoneR = do
  muserId' <- lookupPostParam "userId"
  let muserId'' = fmap unpack muserId'
  let muserId = fmap (readMaybe :: String -> Maybe Int64) muserId''

  mtz' <- lookupPostParam "tz"
  let mtz'' = fmap unpack mtz'
  let mtz   = fmap (readMaybe :: String -> Maybe Int) mtz''
  runDB $ case (muserId, mtz) of
    (Just (Just userId), Just (Just tz)) ->
      update (toSqlKey userId) [UserDueTimeOffset =. Just tz]
    _ -> sendResponseStatus status400 ("bad" :: Text)
  returnJson [("result" :: Text, True)]

