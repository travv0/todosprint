module RepeatIntervalSpec where

import TestImport
import RepeatInterval

spec :: Spec
spec = withApp $
  describe "Weekday" $ do
    it "can be converted to Int" $
      assertEq "Weekdays are correct Ints" (Prelude.map weekdayToInt [Monday .. Sunday]) [1 .. 7]

    it "can be converted from Int" $
      assertEq "Ints are correct Weekdays" (Prelude.map weekdayFromInt [1 .. 7]) [Monday .. Sunday]

    it "can be decremented and loops around" $ do
      assertEq "Previous works non-wrapping" (prev Sunday) Saturday
      assertEq "Previous works wrapping" (prev Monday) Sunday

    it "can be incremented and loops around" $ do
      assertEq "Next works non-wrapping" (next Monday) Tuesday
      assertEq "Next works wrapping" (next Sunday) Monday
