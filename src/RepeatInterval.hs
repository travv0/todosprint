{-# LANGUAGE TemplateHaskell #-}
module RepeatInterval where

import           Database.Persist.TH

data RepeatFrom = CompletionDate | DueDate deriving (Eq, Show, Read)

data RepeatInterval
  = Days Integer RepeatFrom
  | Weeks Integer RepeatFrom
  | Months Integer RepeatFrom
  | Years Integer RepeatFrom
  deriving (Eq, Read, Show)
derivePersistField "RepeatInterval"
