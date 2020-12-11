module App.TaskList where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as AJRF
import Data.Date (Day)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Time (Time)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data RepeatFrom
  = CompletionDate
  | DueDate

data UnitOfTime
  = Days
  | Weeks
  | Months
  | Years

data RepeatInterval
  = ByUnitOfTime UnitOfTime Int RepeatFrom
  | OnWeekdays (Array Weekday)

data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

data Priority
  = None
  | Low
  | Medium
  | High

type Task
  = { name :: String
    , duration :: Int
    , priority :: Priority
    , dueDate :: Maybe Day
    , repeat :: Maybe RepeatInterval
    , done :: Boolean
    , userId :: Int
    , postponeDay :: Maybe Day
    , createTime :: Time
    , doneTime :: Maybe Time
    , deleted :: Boolean
    , deleteTime :: Maybe Time
    }

type State
  = { tasks :: Array Task }

data Action
  = LoadTasks

reset :: forall t1. t1 -> State
reset = \_ -> { tasks: [] }

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: reset
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div [ HP.class_ $ ClassName "tasksDiv" ]
    [ HH.a [ HE.onClick (\_ -> Just LoadTasks) ] [ HH.text "load tasks" ]
    , HH.h4 [ HP.class_ $ ClassName "taskListHeader" ]
        [ HH.text "Today's To-Dos" ]
    , HH.table [ HP.class_ $ ClassName "taskList" ]
        $ flip map state.tasks
        $ \_ ->
            HH.tr [ HP.class_ $ ClassName "task" ]
              [ HH.td [ HP.class_ $ ClassName "taskCheckbox" ]
                  [ HH.form
                      [ HP.action "/mark-done/#"
                      , HP.method $ HP.POST
                      ]
                      [ HH.a
                          [ HP.href "javascript:;"
                          , HP.title "Complete task"
                          ]
                          [ HH.span [ HP.class_ $ ClassName "glyphicon glyphicon-unchecked" ]
                              []
                          ]
                      ]
                  ]
              , HH.td
                  [ HP.class_ $ ClassName "taskInfo"
                  , HP.id_ "task-#"
                  ]
                  [ HH.span [ HP.class_ $ ClassName "taskNameAndDuration" ]
                      [ HH.span [ HP.class_ $ ClassName "taskName" ]
                          [ HH.text "Task Name" ]
                      ]
                  ]
              , HH.td [ HP.class_ $ ClassName "taskManagement" ]
                  [ manageIcon "Edit task" "pencil"
                  , manageIcon "Postpone task" "calendar"
                  , manageIcon "Delete task" "remove"
                  ]
              ]
    ]
  where
  manageIcon title glyphicon =
    HH.a
      [ HP.class_ $ ClassName "manageIcon"
      , HP.href "#"
      , HP.title title
      ]
      [ HH.span
          [ HP.class_ $ ClassName $ "glyphicon glyphicon-" <> glyphicon ]
          []
      ]

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  _ -> H.modify_ identity

-- LoadTasks -> do
--   tasks <- H.liftAff $ AX.get AJRF.json "/tasks"
--   H.modify_ (_ { tasks = map _.body (hush tasks) })
