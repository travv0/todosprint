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
    taskPriority task1 `compare` taskPriority task2 <>
    case (taskDueDate task1, taskDueDate task2) of
      (Nothing, Nothing) -> EQ
      (Just _, Nothing) -> GT
      (Nothing, Just _) -> LT
      (Just dd1, Just dd2) -> dd2 `compare` dd1
    <> taskCreateTime task2 `compare` taskCreateTime task1 <>
    taskDuration task2 `compare` taskDuration task1 <>
    taskName task1 `compare` taskName task2 <>
    taskRepeat task1 `compare` taskRepeat task2 <>
    taskDone task1 `compare` taskDone task2 <>
    taskUserId task1 `compare` taskUserId task2 <>
    taskPostponeDay task1 `compare` taskPostponeDay task2 <>
    taskDoneTime task1 `compare` taskDoneTime task2 <>
    taskDeleted task1 `compare` taskDeleted task2 <>
    taskDeleteTime task1 `compare` taskDeleteTime task2
