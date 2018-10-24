{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.HomeSpec
    ( spec
    )
where

import           TestImport

spec :: Spec
spec = withApp $ do

    describe "Manage page" $ do
        it "asserts redirect to login page from manage page for anonymous users"
            $ do
                  get HomeR
                  statusIs 303

        it "asserts access to manage page for authenticated users" $ do
            userEntity <- createUser "foo@gmail.com"
            authenticateAs userEntity

            get HomeR
            statusIs 200
