{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.TodaySpec
  ( spec
  )
where

import           TestImport
import           Priority
import           Data.Time
import           Handler.Home

spec :: Spec
spec = withApp $ do
  describe "Today page" $ do
    it "isn't detailed task list" $ do
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

      get TodayR
      statusIs 200

      htmlCount ".task" 1
      htmlNoneContain ".taskDate"     "Priority"
      htmlNoneContain ".taskDuration" "minutes"

  describe "sortTasks" $ do
    it "sorts correctly without dependencies" $ do
      currTime <- liftIO getCurrentTime
      let today = utctDay currTime

      userEntity <- createUser "foo@gmail.com"
      runDB $ update (entityKey userEntity)
                     [UserDueTime =. Just (addUTCTime 90 currTime)]

      authenticateAs userEntity

      highPriority <- runDB $ insertEntity $ Task "highPriority"
                                                  30
                                                  High
                                                  (Just today)
                                                  Nothing
                                                  False
                                                  (entityKey userEntity)
                                                  Nothing
                                                  Nothing
                                                  currTime
                                                  Nothing
                                                  False
                                                  Nothing
      highPriorityOverdue <- runDB $ insertEntity $ Task
        "highPriorityOverdue"
        30
        High
        (Just $ addDays (-1) today)
        Nothing
        False
        (entityKey userEntity)
        Nothing
        Nothing
        currTime
        Nothing
        False
        Nothing
      highPriorityWayOverdue <- runDB $ insertEntity $ Task
        "highPriorityWayOverdue"
        30
        High
        (Just $ addDays (-9) today)
        Nothing
        False
        (entityKey userEntity)
        Nothing
        Nothing
        currTime
        Nothing
        False
        Nothing
      mediumPriority <- runDB $ insertEntity $ Task "mediumPriority"
                                                    30
                                                    Medium
                                                    (Just today)
                                                    Nothing
                                                    False
                                                    (entityKey userEntity)
                                                    Nothing
                                                    Nothing
                                                    currTime
                                                    Nothing
                                                    False
                                                    Nothing
      mediumPriorityOverdue <- runDB $ insertEntity $ Task
        "mediumPriorityOverdue"
        30
        Medium
        (Just $ addDays (-1) today)
        Nothing
        False
        (entityKey userEntity)
        Nothing
        Nothing
        currTime
        Nothing
        False
        Nothing
      mediumPriorityWayOverdue <- runDB $ insertEntity $ Task
        "mediumPriorityWayOverdue"
        30
        Medium
        (Just $ addDays (-9) today)
        Nothing
        False
        (entityKey userEntity)
        Nothing
        Nothing
        currTime
        Nothing
        False
        Nothing
      lowPriority <- runDB $ insertEntity $ Task "lowPriority"
                                                 30
                                                 Low
                                                 (Just today)
                                                 Nothing
                                                 False
                                                 (entityKey userEntity)
                                                 Nothing
                                                 Nothing
                                                 currTime
                                                 Nothing
                                                 False
                                                 Nothing
      lowPriorityOverdue <- runDB $ insertEntity $ Task
        "lowPriorityOverdue"
        30
        Low
        (Just $ addDays (-1) today)
        Nothing
        False
        (entityKey userEntity)
        Nothing
        Nothing
        currTime
        Nothing
        False
        Nothing
      lowPriorityWayOverdue <- runDB $ insertEntity $ Task
        "lowPriorityWayOverdue"
        30
        Low
        (Just $ addDays (-9) today)
        Nothing
        False
        (entityKey userEntity)
        Nothing
        Nothing
        currTime
        Nothing
        False
        Nothing
      nonePriority <- runDB $ insertEntity $ Task "nonePriority"
                                                  30
                                                  None
                                                  (Just today)
                                                  Nothing
                                                  False
                                                  (entityKey userEntity)
                                                  Nothing
                                                  Nothing
                                                  currTime
                                                  Nothing
                                                  False
                                                  Nothing
      nonePriorityOverdue <- runDB $ insertEntity $ Task
        "nonePriorityOverdue"
        30
        None
        (Just $ addDays (-1) today)
        Nothing
        False
        (entityKey userEntity)
        Nothing
        Nothing
        currTime
        Nothing
        False
        Nothing
      nonePriorityWayOverdue <- runDB $ insertEntity $ Task
        "nonePriorityWayOverdue"
        30
        None
        (Just $ addDays (-9) today)
        Nothing
        False
        (entityKey userEntity)
        Nothing
        Nothing
        currTime
        Nothing
        False
        Nothing

      let sortableTasks = map
            (\t -> (t, []))
            [ highPriority
            , highPriorityOverdue
            , highPriorityWayOverdue
            , mediumPriority
            , mediumPriorityOverdue
            , mediumPriorityWayOverdue
            , lowPriority
            , lowPriorityOverdue
            , lowPriorityWayOverdue
            , nonePriority
            , nonePriorityOverdue
            , nonePriorityWayOverdue
            ]

      assertEq
        "Not equal: "
        (map (taskName . entityVal) (sortTasks sortableTasks))
        [ "highPriorityWayOverdue"
        , "highPriorityOverdue"
        , "highPriority"
        , "mediumPriorityWayOverdue"
        , "lowPriorityWayOverdue"
        , "mediumPriorityOverdue"
        , "mediumPriority"
        , "lowPriorityOverdue"
        , "lowPriority"
        , "nonePriorityWayOverdue"
        , "nonePriorityOverdue"
        , "nonePriority"
        ]
