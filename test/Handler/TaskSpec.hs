{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.TaskSpec
  ( spec
  )
where

import           TestImport
import           Data.Aeson
import           Priority
import           Data.Time
import qualified Data.List                     as L
import           RepeatInterval

spec :: Spec
spec = withApp $ do

  describe "New task page" $ do
    it "successfully adds tasks" $ do
      userEntity <- createUser "foo@gmail.com"
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
        byLabelExact "Due Date"            "2018-11-03"
        byLabelExact "Start Time"          "17:00"

      statusIs 303
      r <- getResponse
      liftIO $ putStrLn $ pack $ show r

      [Entity _ task] <- runDB $ selectList ([] :: [Filter Task]) []

      assertEq "Task wasn't added correctly"
               (task { taskCreateTime = currTime })
        $ Task
            "asdf"
            4
            High
            (fromGregorianValid 2018 11 3)
            (   UTCTime
            <$> (fromGregorianValid 2018 11 3)
            <*> (timeOfDayToTime <$> makeTimeOfDayValid 17 0 0)
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
