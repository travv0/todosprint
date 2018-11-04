{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.TaskSpec
  ( spec
  )
where

import           TestImport
import           Priority
import           Data.Time
import           Common
import           Data.Maybe                    as M

spec :: Spec
spec = withApp $ do

  describe "New task page" $ do
    it "successfully adds tasks" $ do
      userEntity' <- createUser "foo@gmail.com"
      runDB $ update (entityKey userEntity') [UserDueTimeOffset =. Just (-300)]
      mUserEntity <- runDB $ getEntity (entityKey userEntity')
      let userEntity = M.fromJust mUserEntity
      authenticateAs userEntity
      currTime <- liftIO getCurrentTime

      get NewTaskR
      statusIs 200

      request $ do
        setMethod "POST"
        setUrl NewTaskR
        addToken

        byLabelExact "Task Name"           "asdf"
        byLabelExact "Duration in Minutes" "4"
        byLabelExact "Priority"            "4"
        byLabelExact "Due Date"            "2008-11-03"
        byLabelExact "Start Time"          "23:00"

      statusIs 303
      r <- getResponse
      liftIO $ putStrLn $ pack $ show r

      [Entity _ task] <- runDB $ selectList ([] :: [Filter Task]) []

      let userTz = userTimeZoneOrUtc (entityVal userEntity)

      assertEq "Task wasn't added correctly"
               (task { taskCreateTime = currTime })
        $ Task
            "asdf"
            4
            High
            (fromGregorianValid 2008 11 3)
            (   localTimeToUTC userTz
            <$> (   LocalTime
                <$> fromGregorianValid 2008 11 3
                <*> makeTimeOfDayValid 23 0 0
                )
            )
            Nothing
            False
            (entityKey userEntity)
            Nothing
            currTime
            Nothing
            False
            Nothing
            True

  describe "Marking task done" $ do
    it "works for repeating task by days" $ do
      currTime <- liftIO getCurrentTime
      let today = utctDay currTime

      userEntity <- createUser "foo@gmail.com"
      authenticateAs userEntity

      highPriority <- runDB $ insertEntity $ Task
        "highPriority"
        1
        High
        (Just today)
        (Just (addUTCTime 59 currTime))
        Nothing
        False
        (entityKey userEntity)
        Nothing
        currTime
        Nothing
        False
        Nothing
        False

      request $ do
        setMethod "POST"
        setUrl $ MarkDoneR $ entityKey highPriority

      r <- getResponse
      liftIO $ putStrLn $ pack $ show r
      statusIs 303

      hpTasks <- runDB
        $ selectList [TaskName ==. "highPriority", TaskDone ==. False] []
      assertEq "Task with no repeat was repeated" (hpTasks :: [Entity Task]) []
