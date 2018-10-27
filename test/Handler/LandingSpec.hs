{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.LandingSpec
  ( spec
  )
where

import           TestImport

spec :: Spec
spec = withApp $ do
  describe "Landing page" $ do
    it "doesn't redirect if logged out" $ do
      get LandingR
      statusIs 200

    it "redirects if already logged in" $ do
      userEntity <- createUser "foo@gmail.com"
      authenticateAs userEntity

      get LandingR
      statusIs 303
