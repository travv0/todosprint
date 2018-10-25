{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

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

    it "sorts correctly without dependencies" $ do
      currTime      <- liftIO getCurrentTime
      currTimeLater <- liftIO getCurrentTime
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
      highPriorityNoDueDate <- runDB $ insertEntity $ Task
        "highPriorityNoDueDate"
        30
        High
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
      mediumPriorityNoDueDate <- runDB $ insertEntity $ Task
        "mediumPriorityNoDueDate"
        30
        Medium
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
      mediumPriorityNoDueDateShort <- runDB $ insertEntity $ Task
        "mediumPriorityNoDueDateShort"
        5
        Medium
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
      mediumPriorityNoDueDateShortAddedLater <- runDB $ insertEntity $ Task
        "mediumPriorityNoDueDateShortAddedLater"
        5
        Medium
        Nothing
        Nothing
        False
        (entityKey userEntity)
        Nothing
        Nothing
        currTimeLater
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
      lowPriorityNoDueDate <- runDB $ insertEntity $ Task
        "lowPriorityNoDueDate"
        30
        Low
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
      nonePriorityNoDueDate <- runDB $ insertEntity $ Task
        "nonePriorityNoDueDate"
        30
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
            , highPriorityNoDueDate
            , mediumPriorityNoDueDate
            , lowPriorityNoDueDate
            , nonePriorityNoDueDate
            , mediumPriorityNoDueDateShortAddedLater
            , mediumPriorityNoDueDateShort
            ]

      assertEq
        "Not equal: "
        (map (taskName . entityVal) (sortTasks sortableTasks))
        [ "highPriorityWayOverdue"
        , "highPriorityOverdue"
        , "highPriority"
        , "highPriorityNoDueDate"
        , "mediumPriorityWayOverdue"
        , "mediumPriorityOverdue"
        , "mediumPriority"
        , "mediumPriorityNoDueDateShort"
        , "mediumPriorityNoDueDate"
        , "mediumPriorityNoDueDateShortAddedLater"
        , "lowPriorityWayOverdue"
        , "lowPriorityOverdue"
        , "lowPriority"
        , "lowPriorityNoDueDate"
        , "nonePriorityWayOverdue"
        , "nonePriorityOverdue"
        , "nonePriority"
        , "nonePriorityNoDueDate"
        ]

    it "sorts correctly with dependencies" $ do
      currTime      <- liftIO getCurrentTime
      currTimeLater <- liftIO getCurrentTime
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
      highPriorityNoDueDate <- runDB $ insertEntity $ Task
        "highPriorityNoDueDate"
        30
        High
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
      mediumPriorityNoDueDate <- runDB $ insertEntity $ Task
        "mediumPriorityNoDueDate"
        30
        Medium
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
      mediumPriorityNoDueDateShort <- runDB $ insertEntity $ Task
        "mediumPriorityNoDueDateShort"
        5
        Medium
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
      mediumPriorityNoDueDateShortAddedLater <- runDB $ insertEntity $ Task
        "mediumPriorityNoDueDateShortAddedLater"
        5
        Medium
        Nothing
        Nothing
        False
        (entityKey userEntity)
        Nothing
        Nothing
        currTimeLater
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
      lowPriorityNoDueDate <- runDB $ insertEntity $ Task
        "lowPriorityNoDueDate"
        30
        Low
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
      nonePriorityNoDueDate <- runDB $ insertEntity $ Task
        "nonePriorityNoDueDate"
        30
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

      let sortableTasks = map
            (\t -> (t, if
                        | t == lowPriority -> [highPriority]
                        | t == highPriority -> [mediumPriority, highPriorityWayOverdue]
                        | t == highPriorityNoDueDate -> [highPriority]
                        | otherwise -> []))
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
            , highPriorityNoDueDate
            , mediumPriorityNoDueDate
            , lowPriorityNoDueDate
            , nonePriorityNoDueDate
            , mediumPriorityNoDueDateShortAddedLater
            , mediumPriorityNoDueDateShort
            ]

      assertEq
        "Not equal: "
        (map (taskName . entityVal) (sortTasks sortableTasks))
        [ "lowPriority"
        , "highPriorityNoDueDate"
        , "highPriority"
        , "highPriorityWayOverdue"
        , "highPriorityOverdue"
        , "mediumPriorityWayOverdue"
        , "mediumPriorityOverdue"
        , "mediumPriority"
        , "mediumPriorityNoDueDateShort"
        , "mediumPriorityNoDueDate"
        , "mediumPriorityNoDueDateShortAddedLater"
        , "lowPriorityWayOverdue"
        , "lowPriorityOverdue"
        , "lowPriorityNoDueDate"
        , "nonePriorityWayOverdue"
        , "nonePriorityOverdue"
        , "nonePriority"
        , "nonePriorityNoDueDate"
        ]

    it "weighs tasks correctly relative to each other" $ do
      currTime      <- liftIO getCurrentTime
      currTimeLater <- liftIO getCurrentTime
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
      highPriorityNoDueDate <- runDB $ insertEntity $ Task
        "highPriorityNoDueDate"
        30
        High
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
      mediumPriorityNoDueDate <- runDB $ insertEntity $ Task
        "mediumPriorityNoDueDate"
        30
        Medium
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
      mediumPriorityNoDueDateShort <- runDB $ insertEntity $ Task
        "mediumPriorityNoDueDateShort"
        5
        Medium
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
      mediumPriorityNoDueDateShortAddedLater <- runDB $ insertEntity $ Task
        "mediumPriorityNoDueDateShortAddedLater"
        5
        Medium
        Nothing
        Nothing
        False
        (entityKey userEntity)
        Nothing
        Nothing
        currTimeLater
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
      lowPriorityNoDueDate <- runDB $ insertEntity $ Task
        "lowPriorityNoDueDate"
        30
        Low
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
        (Just $ addDays (-900) today)
        Nothing
        False
        (entityKey userEntity)
        Nothing
        Nothing
        currTime
        Nothing
        False
        Nothing
      nonePriorityNoDueDate <- runDB $ insertEntity $ Task
        "nonePriorityNoDueDate"
        30
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

      assertEq
        "High priority should have higher weight than medium"
        ( weight today (entityVal highPriority)
        > weight today (entityVal mediumPriority)
        )
        True
      assertEq
        "Medium priority should have higher weight than low"
        ( weight today (entityVal mediumPriority)
        > weight today (entityVal lowPriority)
        )
        True
      assertEq
        "Low priority should have higher weight than none"
        ( weight today (entityVal lowPriority)
        > weight today (entityVal nonePriority)
        )
        True
      assertEq
        "Low priority overdue by longer than a week should have higher weight than medium"
        ( weight today (entityVal lowPriorityWayOverdue)
        > weight today (entityVal mediumPriority)
        )
        True
      assertEq
        "Medium priority overdue by longer than a week should not have higher weight than high"
        ( weight today (entityVal mediumPriorityWayOverdue)
        > weight today (entityVal highPriority)
        )
        False
      assertEq
        "None priority should never have higher weight than anything else"
        ( weight today (entityVal nonePriorityWayOverdue)
        > weight today (entityVal lowPriority)
        )
        False
      assertEq
        "High priority should be weighted by how overdue"
        ( weight today (entityVal highPriorityWayOverdue)
        > weight today (entityVal highPriorityOverdue)
        )
        True
      assertEq
        "Tasks with no due date should be weighted lower than those with due dates, unless none priority"
        (  ( weight today (entityVal highPriority)
           > weight today (entityVal highPriorityNoDueDate)
           )
        && ( weight today (entityVal mediumPriority)
           > weight today (entityVal mediumPriorityNoDueDate)
           )
        && ( weight today (entityVal lowPriority)
           > weight today (entityVal lowPriorityNoDueDate)
           )
        && (  weight today (entityVal nonePriority)
           == weight today (entityVal nonePriorityNoDueDate)
           )
        )
        True
      assertEq
        "None priority should always be the same weight as each other"
        (all
          (== 0)
          (map
            (weight today . entityVal)
            [ nonePriority
            , nonePriorityOverdue
            , nonePriorityWayOverdue
            , nonePriorityNoDueDate
            ]
          )
        )
        True
      assertEq
        "Task create date shouldn't affect weight"
        (  weight today (entityVal mediumPriorityNoDueDateShort)
        == weight today (entityVal mediumPriorityNoDueDateShortAddedLater)
        )
        True
      assertEq
        "Task duration shouldn't affect weight"
        (  weight today (entityVal mediumPriorityNoDueDateShort)
        == weight today (entityVal mediumPriorityNoDueDate)
        )
        True

    it "calculates time to complete tasks correctly" $ do
      currTime <- liftIO getCurrentTime
      let today = utctDay currTime

      userEntity <- createUser "foo@gmail.com"
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
      highPriorityNoDueDate <- runDB $ insertEntity $ Task
        "highPriorityNoDueDate"
        30
        High
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

      assertEq
        "Time to complete 4 30-minute tasks should be 120"
        (timeToComplete
          [ highPriority
          , highPriorityOverdue
          , highPriorityNoDueDate
          , highPriorityWayOverdue
          ]
        )
        120

    it "correctly filters tasks postponed by time and their dependents" $ do
      currTime <- liftIO getCurrentTime
      let today = utctDay currTime

      userEntity <- createUser "foo@gmail.com"

      authenticateAs userEntity

      highPriority <- runDB $ insertEntity $ Task
        "highPriority"
        30
        High
        (Just today)
        Nothing
        False
        (entityKey userEntity)
        (Just (addUTCTime 30 currTime))
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
      highPriorityNoDueDate <- runDB $ insertEntity $ Task
        "highPriorityNoDueDate"
        30
        High
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

      runDB $ insert $ TaskDependency (entityKey highPriorityNoDueDate)
                                      (entityKey highPriorityOverdue)
                                      False
      runDB $ insert $ TaskDependency (entityKey highPriorityOverdue)
                                      (entityKey highPriority)
                                      False

      postponedTasks <- runHandler $ postponedTaskList
        currTime
        [ highPriority
        , highPriorityOverdue
        , highPriorityNoDueDate
        , highPriorityWayOverdue
        ]

      assertEq
        "Postponed tasks"
        (map (taskName . entityVal) (sort postponedTasks))
        (map (taskName . entityVal)
             (sort [highPriority, highPriorityOverdue, highPriorityNoDueDate])
        )
