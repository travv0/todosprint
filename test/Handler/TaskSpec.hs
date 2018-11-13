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
import qualified Data.List                     as L

spec :: Spec
spec = withApp $ do
  describe "Add dependencies page" $ do
    it "doesn't say dependent anywhere" $ do
      currTime   <- liftIO getCurrentTime

      userEntity <- createUser "foo@gmail.com"

      authenticateAs userEntity

      testTask <- runDB $ insert $ Task "Test"
                                        1
                                        High
                                        Nothing
                                        Nothing
                                        Nothing
                                        False
                                        (entityKey userEntity)
                                        Nothing
                                        currTime
                                        Nothing
                                        False
                                        Nothing
                                        False
                                        True

      get $ AddDepsR testTask
      statusIs 200

      bodyNotContains "dependent"
      bodyNotContains "Dependent"
      bodyContains "Dependenc"

    it "adds dependencies correctly" $ do
      currTime   <- liftIO getCurrentTime

      userEntity <- createUser "foo@gmail.com"

      authenticateAs userEntity

      testTask <- runDB $ insert $ Task "Test"
                                        1
                                        High
                                        Nothing
                                        Nothing
                                        Nothing
                                        False
                                        (entityKey userEntity)
                                        Nothing
                                        currTime
                                        Nothing
                                        False
                                        Nothing
                                        False
                                        True
      depTask <- runDB $ insert $ Task "dep"
                                       1
                                       High
                                       Nothing
                                       Nothing
                                       Nothing
                                       False
                                       (entityKey userEntity)
                                       Nothing
                                       currTime
                                       Nothing
                                       False
                                       Nothing
                                       False
                                       True
      _ <- runDB $ insert $ Task "nothing"
                                 1
                                 High
                                 Nothing
                                 Nothing
                                 Nothing
                                 False
                                 (entityKey userEntity)
                                 Nothing
                                 currTime
                                 Nothing
                                 False
                                 Nothing
                                 False
                                 True
      _ <- runDB $ insert $ Task "nothing2"
                                 1
                                 High
                                 Nothing
                                 Nothing
                                 Nothing
                                 False
                                 (entityKey userEntity)
                                 Nothing
                                 currTime
                                 Nothing
                                 False
                                 Nothing
                                 False
                                 True

      get $ AddDepsR testTask
      statusIs 200

      printBody

      request $ do
        setMethod "POST"
        setUrl $ AddDepsR testTask
        addToken
        byLabelExact "Dependencies" "1"

      deps <- runDB $ selectList [TaskDependencyTaskId ==. testTask] []
      assertEq "One dependency" (length deps) 1
      assertEq ("Correct dependency for " ++ show testTask)
               (taskDependencyDependsOnTaskId $ entityVal $ L.head deps)
               depTask

  describe "Add dependents page" $ do
    it "doesn't say dependency/dependencies anywhere" $ do
      currTime   <- liftIO getCurrentTime

      userEntity <- createUser "foo@gmail.com"

      authenticateAs userEntity

      testTask <- runDB $ insert $ Task "Test"
                                        1
                                        High
                                        Nothing
                                        Nothing
                                        Nothing
                                        False
                                        (entityKey userEntity)
                                        Nothing
                                        currTime
                                        Nothing
                                        False
                                        Nothing
                                        False
                                        True

      get $ AddDependentsR testTask
      statusIs 200

      printBody

      bodyNotContains "dependenc"
      bodyNotContains "Dependenc"
      bodyContains "Dependent"

    it "adds dependents correctly" $ do
      currTime   <- liftIO getCurrentTime

      userEntity <- createUser "foo@gmail.com"

      authenticateAs userEntity

      testTask <- runDB $ insert $ Task "Test"
                                        1
                                        High
                                        Nothing
                                        Nothing
                                        Nothing
                                        False
                                        (entityKey userEntity)
                                        Nothing
                                        currTime
                                        Nothing
                                        False
                                        Nothing
                                        False
                                        True
      depTask <- runDB $ insert $ Task "dep"
                                       1
                                       High
                                       Nothing
                                       Nothing
                                       Nothing
                                       False
                                       (entityKey userEntity)
                                       Nothing
                                       currTime
                                       Nothing
                                       False
                                       Nothing
                                       False
                                       True
      _ <- runDB $ insert $ Task "nothing"
                                 1
                                 High
                                 Nothing
                                 Nothing
                                 Nothing
                                 False
                                 (entityKey userEntity)
                                 Nothing
                                 currTime
                                 Nothing
                                 False
                                 Nothing
                                 False
                                 True
      _ <- runDB $ insert $ Task "nothing2"
                                 1
                                 High
                                 Nothing
                                 Nothing
                                 Nothing
                                 False
                                 (entityKey userEntity)
                                 Nothing
                                 currTime
                                 Nothing
                                 False
                                 Nothing
                                 False
                                 True

      get $ AddDependentsR testTask
      statusIs 200

      printBody

      request $ do
        setMethod "POST"
        setUrl $ AddDependentsR testTask
        addToken
        byLabelExact "Dependents" "1"

      deps <- runDB $ selectList [TaskDependencyDependsOnTaskId ==. testTask] []
      assertEq "One dependency" (length deps) 1
      assertEq ("Correct dependent for " ++ show testTask)
               (taskDependencyTaskId $ entityVal $ L.head deps)
               depTask

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
            False

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
        True

      request $ do
        setMethod "POST"
        setUrl $ MarkDoneR $ entityKey highPriority

      r <- getResponse
      liftIO $ putStrLn $ pack $ show r
      statusIs 303

      hpTasks <- runDB
        $ selectList [TaskName ==. "highPriority", TaskDone ==. False] []
      assertEq "Task with no repeat was repeated" (hpTasks :: [Entity Task]) []
