{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.TaskSpec (
    spec,
) where

import qualified Data.List as List
import Data.Maybe as M
import Data.Time
import Priority
import TestImport

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

                byLabelExact "Task Name" "asdf"
                byLabelExact "Duration in Minutes" "4"
                byLabelExact "Priority" "4"
                byLabelExact "Due Date" "2008-11-03"

            statusIs 303
            r <- getResponse
            liftIO $ putStrLn $ pack $ show r

            entity <- runDB $ selectList ([] :: [Filter Task]) []
            let Entity _ task = List.head entity

            assertEq
                "Task wasn't added correctly"
                (task{taskCreateTime = currTime})
                $ Task
                    "asdf"
                    4
                    High
                    (fromGregorianValid 2008 11 3)
                    Nothing
                    False
                    (entityKey userEntity)
                    Nothing
                    currTime
                    Nothing
                    False
                    Nothing
                    False

    describe "Marking task done" $ do
        it "works for repeating task by days" $ do
            currTime <- liftIO getCurrentTime
            let today = utctDay currTime

            userEntity <- createUser "foo@gmail.com"
            authenticateAs userEntity

            highPriority <-
                runDB $
                    insertEntity $
                        Task
                            "highPriority"
                            1
                            High
                            (Just today)
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

            hpTasks <-
                runDB $
                    selectList [TaskName ==. "highPriority", TaskDone ==. False] []
            assertEq "Task with no repeat was repeated" (hpTasks :: [Entity Task]) []
