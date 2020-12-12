{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RepeatInterval where

import ClassyPrelude
import Database.Persist.TH

data RepeatFrom = CompletionDate | DueDate deriving (Eq, Show, Read, Enum, Bounded, Ord)

data Weekday
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Eq, Read, Show, Ord, Enum, Bounded)

prev :: forall a. (Enum a, Bounded a) => a -> a
prev x
    | from x > from minBound = pred x
    | otherwise = maxBound
  where
    from :: a -> Int
    from = fromEnum

next :: forall a. (Enum a, Bounded a) => a -> a
next x
    | from x < from maxBound = succ x
    | otherwise = minBound
  where
    from :: a -> Int
    from = fromEnum

weekdayToInt :: Weekday -> Int
weekdayToInt wkday = fromEnum wkday `mod` 7 + 1

weekdayFromInt :: Int -> Weekday
weekdayFromInt i = toEnum ((i + 7 - 1) `mod` 7)

data UnitOfTime = Days | Weeks | Months | Years
    deriving (Eq, Read, Ord, Show)

data RepeatInterval
    = ByUnitOfTime UnitOfTime Integer RepeatFrom
    | OnWeekdays [Weekday]
    deriving (Eq, Read, Show, Ord)
derivePersistField "RepeatInterval"

formatRepeatInterval :: RepeatInterval -> Text
formatRepeatInterval (ByUnitOfTime unit i from) =
    "Every " <> pack (show i) <> " "
        <> pack (toLower (show unit))
        <> " from "
        <> formatRepeatFrom from
  where
    formatRepeatFrom CompletionDate = "completion date"
    formatRepeatFrom DueDate = "due date"
formatRepeatInterval (OnWeekdays weekdays) =
    "Every " <> pack (intercalate ", " (map show weekdays))
