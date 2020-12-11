module Main where

import Prelude
import App.TaskList as TaskList
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))

taskList :: QuerySelector -> Aff Unit
taskList selector = do
  element <- HA.selectElement selector
  case element of
    Just e -> void $ runUI TaskList.component unit e
    Nothing -> pure unit

main :: Effect Unit
main =
  HA.runHalogenAff do
    taskList $ QuerySelector "#psTaskList"
