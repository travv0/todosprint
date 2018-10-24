{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.TodaySpec
    ( spec
    )
where

import           TestImport
import           Priority
import           Data.Time

spec :: Spec
spec = withApp $ do
    describe "Today page" $ do
        it "isn't detailed task list" $ do
            currTime <- liftIO getCurrentTime

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

            get TodayR
            statusIs 200

            htmlCount ".task" 1
            htmlNoneContain ".taskDate" "Priority"
            htmlNoneContain ".taskDuration" "minutes"
