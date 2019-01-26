{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import           ClassyPrelude.Yesod
import           Database.Persist.Quasi
import           Priority
import           RepeatInterval

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")

-- reversed so it works right with topsort
instance Ord Task where
  task1 `compare` task2 =
    taskPriority task2 `compare` taskPriority task1 <>
    case (taskDueDate task1, taskDueDate task2) of
      (Nothing, Nothing) -> EQ
      (Just _, Nothing) -> LT
      (Nothing, Just _) -> GT
      (Just dd1, Just dd2) -> dd1 `compare` dd2
    <> taskCreateTime task1 `compare` taskCreateTime task2 <>
    taskDuration task1 `compare` taskDuration task2 <>
    taskName task2 `compare` taskName task1 <>
    taskRepeat task2 `compare` taskRepeat task1 <>
    taskDone task2 `compare` taskDone task1 <>
    taskUserId task2 `compare` taskUserId task1 <>
    taskPostponeDay task2 `compare` taskPostponeDay task1 <>
    taskDoneTime task2 `compare` taskDoneTime task1 <>
    taskDeleted task2 `compare` taskDeleted task1 <>
    taskDeleteTime task2 `compare` taskDeleteTime task1
