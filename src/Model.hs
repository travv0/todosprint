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

instance Ord Task where
  task1 `compare` task2 =
    taskPriority task2 `compare` taskPriority task1 <>
    taskDueDate task1 `compare` taskDueDate task2 <>
    taskDuration task1 `compare` taskDuration task2 <>
    taskName task1 `compare` taskName task2 <>
    taskRepeat task1 `compare` taskRepeat task2 <>
    taskDone task1 `compare` taskDone task2 <>
    taskUserId task1 `compare` taskUserId task2 <>
    taskPostponeTime task1 `compare` taskPostponeTime task2 <>
    taskPostponeDay task1 `compare` taskPostponeDay task2 <>
    taskCreateTime task1 `compare` taskCreateTime task2 <>
    taskDoneTime task1 `compare` taskDoneTime task2 <>
    taskDeleted task1 `compare` taskDeleted task2 <>
    taskDeleteTime task1 `compare` taskDeleteTime task2
