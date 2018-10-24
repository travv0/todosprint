{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.HomeSpec
  ( spec
  )
where

import           TestImport
import           Priority
import           Data.Time

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

    it "has detailed task list" $ do
      currTime   <- liftIO getCurrentTime

      userEntity <- createUser "foo@gmail.com"
      runDB $ update (entityKey userEntity)
                     [UserDueTime =. Just (addUTCTime 90 currTime)]

      authenticateAs userEntity

      testTask <- runDB $ insert $ Task "Test"
                                        1
                                        None
                                        Nothing
                                        Nothing
                                        False
                                        (entityKey userEntity)
                                        Nothing
                                        Nothing
                                        currTime
                                        Nothing
                                        False
                                        Nothing

      get HomeR
      statusIs 200

      htmlCount ".task" 1
      htmlAllContain ".taskDate"     "Priority"
      htmlAllContain ".taskDuration" "minutes"
