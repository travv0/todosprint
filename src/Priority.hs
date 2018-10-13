{-# LANGUAGE TemplateHaskell #-}
module Priority where

import Database.Persist.TH

data Priority
  = None
  | Low
  | Medium
  | High
  deriving (Eq, Show, Ord, Read, Enum)
derivePersistField "Priority"
