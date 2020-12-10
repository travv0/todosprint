module App.TaskList where

import Prelude
import Data.Array ((..))
import Halogen as H
import Halogen.HTML (AttrName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties (attr)

type State
  = { count :: Int }

data Action
  = Increment
  | Decrement
  | Reset

reset :: forall t1. t1 -> State
reset = \_ -> { count: 0 }

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: reset
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div [ attr (AttrName "class") "tasksDiv" ]
    [ HH.h4 [ attr (AttrName "class") "taskListHeader" ]
        [ HH.text "Today's To-Dos" ]
    , HH.table [ attr (AttrName "class") "taskList" ]
        $ flip map (0 .. 3)
        $ \_ ->
            HH.tr [ attr (AttrName "class") "task" ]
              [ HH.td [ attr (AttrName "class") "taskCheckbox" ]
                  [ HH.form
                      [ attr (AttrName "action") "/mark-done/#"
                      , attr (AttrName "method") "post"
                      ]
                      [ HH.a
                          [ attr (AttrName "href") "javascript:;"
                          , attr (AttrName "title") "Complete task"
                          , attr (AttrName "onclick") "this.parentNode.submit();"
                          ]
                          [ HH.span
                              [ attr (AttrName "class") "glyphicon glyphicon-unchecked"
                              , attr (AttrName "aria-hidden") "true"
                              ]
                              []
                          ]
                      ]
                  ]
              , HH.td
                  [ attr (AttrName "class") "taskInfo"
                  , attr (AttrName "id") "task-#"
                  ]
                  [ HH.span [ attr (AttrName "class") "taskNameAndDuration" ]
                      [ HH.span [ attr (AttrName "class") "taskName" ]
                          [ HH.text "Task Name" ]
                      ]
                  ]
              , HH.td [ attr (AttrName "class") "taskManagement" ]
                  [ manageIcon "Edit task" "pencil"
                  , manageIcon "Postpone task" "calendar"
                  , manageIcon "Delete task" "remove"
                  ]
              ]
    ]
  where
  manageIcon title glyphicon =
    HH.a
      [ attr (AttrName "class") "manageIcon"
      , attr (AttrName "href") "#"
      , attr (AttrName "title") title
      ]
      [ HH.span
          [ attr (AttrName "class") $ "glyphicon glyphicon-" <> glyphicon
          , attr (AttrName "aria-hidden") "true"
          ]
          []
      ]

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Increment -> H.modify_ \st -> st { count = st.count + 1 }
  Decrement -> H.modify_ \st -> st { count = st.count - 1 }
  Reset -> H.modify_ reset
