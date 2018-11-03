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

spec :: Spec
spec = withApp $ do

  describe "New task page" $ do
    it "successfully adds tasks" $ do
      userEntity <- createUser "foo@gmail.com"
      authenticateAs userEntity

      get NewTaskR
      statusIs 200

      authenticateAs userEntity

      request $ do
        setMethod "POST"
        setUrl NewTaskR
        addPostParam "hident2" "asdf"
        addPostParam "hident3" "4"
        addPostParam "hident4" "4"
        addPostParam "hident5" "2018-11-03"
        addPostParam "hident6-startTime" "17:00"
        addPostParam "hident7-count" "4"
        addPostParam "hident7-unit" "Days"
        addPostParam "hident7-from" "CompletionDate"

      r <- getResponse
      liftIO $ putStrLn $ pack $ show r
      statusIs 200

  describe "Marking task done" $ do
    it "works for repeating task by days" $ do
      currTime   <- liftIO getCurrentTime
      let today = utctDay currTime

      userEntity <- createUser "foo@gmail.com"
      authenticateAs userEntity

      highPriority <- runDB $ insertEntity $ Task "highPriority"
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

      hpTasks <- runDB $ selectList [TaskName ==. "highPriority", TaskDone ==. False] []
      assertEq "Task with no repeat was repeated" (hpTasks :: [Entity Task]) []
