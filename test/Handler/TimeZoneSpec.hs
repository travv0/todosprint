{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.TimeZoneSpec
  ( spec
  )
where

import           TestImport

spec :: Spec
spec = withApp $ do
  describe "TimeZone page" $ do
    it "redirects to Today when GET" $ do
      get TimeZoneR
      statusIs 303
