{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Priority where

import ClassyPrelude.Yesod

data Priority
  = None
  | Low
  | Medium
  | High
  deriving (Eq, Show, Ord, Read, Enum, Bounded, Generic)
derivePersistField "Priority"

instance FromJSON Priority
instance ToJSON Priority
