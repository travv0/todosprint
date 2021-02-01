module App.TaskList where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as AJRF
import Data.Argonaut (Json)
import Data.Argonaut as A
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Date as D
import Data.DateTime (date)
import Data.DateTime as DT
import Data.Either (Either(..), hush)
import Data.Formatter.DateTime (unformatDateTime)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), indexOf, length, splitAt)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (toEvent)

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

newtype DateTime
  = DateTime DT.DateTime

instance decodeJsonDateTime :: DecodeJson DateTime where
  decodeJson = fromString <=< decodeJson
    where
    fromString :: String -> Either JsonDecodeError DateTime
    fromString s =
      ( case _ of
          Right dateTime -> Right $ DateTime dateTime
          Left err -> Left (TypeMismatch err)
          <<< unformatDateTime "YYYY-MM-DDTHH:mm:ss"
          <<< _.before
          <<< flip splitAt s
          <<< fromMaybe (length s)
          <<< indexOf (Pattern ".")
      )
        s

derive newtype instance showDateTime :: Show DateTime

newtype Date
  = Date D.Date

instance decodeJsonDate :: DecodeJson Date where
  decodeJson = fromString <=< decodeJson
    where
    fromString :: String -> Either JsonDecodeError Date
    fromString =
      case _ of
        Right dateTime -> Right $ Date $ date dateTime
        Left err -> Left (TypeMismatch err)
        <<< (unformatDateTime "YYYY-MM-DD")

derive newtype instance showDate :: Show Date

type Task
  = { id :: Int
    , name :: String
    , duration :: Int
    , priority :: Priority
    , dueDate :: Maybe Date
    , repeat :: Maybe RepeatInterval
    , done :: Boolean
    , userId :: Int
    , postponeDay :: Maybe Date
    , createTime :: DateTime
    , doneTime :: Maybe DateTime
    , deleted :: Boolean
    , deleteTime :: Maybe DateTime
    }

derive instance genericPriority :: Generic Priority _

instance showPriority :: Show Priority where
  show = genericShow

instance decodeJsonPriority :: DecodeJson Priority where
  decodeJson = \_ -> Right High

-- decodeJson = case _ of
--   "None" -> None
--   "Low" -> Low
--   "Medium" -> Medium
--   "High" -> High
--   other -> TypeMismatch $ "Invalid Priority: " <> other
derive instance genericRepeatInterval :: Generic RepeatInterval _

instance showRepeatInterval :: Show RepeatInterval where
  show = genericShow

instance decodeJsonRepeatInterval :: DecodeJson RepeatInterval where
  decodeJson = genericDecodeJson

derive instance genericUnitOfTime :: Generic UnitOfTime _

instance showUnitOfTime :: Show UnitOfTime where
  show = genericShow

instance decodeJsonUnitOfTime :: DecodeJson UnitOfTime where
  decodeJson = genericDecodeJson

derive instance genericRepeatFrom :: Generic RepeatFrom _

instance showRepeatFrom :: Show RepeatFrom where
  show = genericShow

instance decodeJsonRepeatFrom :: DecodeJson RepeatFrom where
  decodeJson = genericDecodeJson

derive instance genericWeekday :: Generic Weekday _

instance showWeekday :: Show Weekday where
  show = genericShow

instance decodeJsonWeekday :: DecodeJson Weekday where
  decodeJson = genericDecodeJson

taskFromJson :: Json -> Either JsonDecodeError Task
taskFromJson = A.decodeJson

type State
  = { tasks :: Maybe (Array Task) }

data Action
  = LoadTasks Event

reset :: forall t1. t1 -> State
reset = \_ -> { tasks: Nothing }

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: reset
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div [ HP.class_ $ ClassName "tasksDiv" ]
    [ HH.a [ HE.onClick (Just <<< LoadTasks <<< toEvent) ] [ HH.text "load tasks" ]
    , HH.h4 [ HP.class_ $ ClassName "taskListHeader" ]
        [ HH.text "Today's To-Dos" ]
    , HH.table [ HP.class_ $ ClassName "taskList" ] case state.tasks of
        Nothing -> [ HH.text "No tasks" ]
        Just tasks ->
          map
            ( \task ->
                HH.tr [ HP.class_ $ ClassName "task" ]
                  [ HH.td [ HP.class_ $ ClassName "taskCheckbox" ]
                      [ HH.form
                          [ HP.action $ "/mark-done/" <> show task.id
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
                      , HP.id_ $ "task-" <> show task.id
                      ]
                      [ HH.span [ HP.class_ $ ClassName "taskNameAndDuration" ]
                          [ HH.span [ HP.class_ $ ClassName "taskName" ]
                              [ HH.text $ task.name ]
                          ]
                      ]
                  , HH.td [ HP.class_ $ ClassName "taskManagement" ]
                      [ manageIcon "Edit task" "pencil"
                      , manageIcon "Postpone task" "calendar"
                      , manageIcon "Delete task" "remove"
                      ]
                  ]
            )
            tasks
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

handleAction :: forall cs o m. MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  LoadTasks event -> do
    H.liftEffect $ Event.preventDefault event
    tasksRequest <- H.liftAff $ AX.get AJRF.json "/tasks"
    let
      json = map (A.decodeJson <<< _.body) (hush tasksRequest)
    H.liftEffect $ log $ show json
    let
      tasks = join $ map hush json
    H.liftEffect $ log $ show tasks
    H.modify_ (_ { tasks = tasks })
