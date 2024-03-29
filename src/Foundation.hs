{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Foundation where

import Control.Monad.Logger (LogSource)
import Data.Maybe
import Database.Persist.Sql (ConnectionPool, fromSqlKey, runSqlPool)
import Import.NoFoundation
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Text.Julius (RawJS (..))

import Data.Aeson (withObject)
import qualified Data.CaseInsensitive as CI
import Data.Kind (Type)
import qualified Data.Text.Encoding as TE
import Yesod.Auth.Dummy
import Yesod.Auth.OAuth2 (getUserResponseJSON, oauth2Url)
import Yesod.Auth.OAuth2.Google (oauth2Google)
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Form.Jquery

{- | The foundation datatype for your application. This can be a good place to
 keep settings and values requiring initialization before your application
 starts running, such as database connections. Every handler will have
 access to the data present here.
-}
data App = App
    { appSettings :: AppSettings
    , -- | Settings for static file serving.
      appStatic :: Static
    , -- | Database connection pool.
      appConnPool :: ConnectionPool
    , appHttpManager :: Manager
    , appLogger :: Logger
    , googleClientId :: Text
    , googleClientSecret :: Text
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a =
    forall (m :: Type -> Type).
    (MonadIO m) =>
    ReaderT SqlBackend m a

instance YesodJquery App

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot

    approot :: Approot App
    approot =
        ApprootRequest $ \app req ->
            case appRoot $ appSettings app of
                Nothing -> getApprootText guessApproot app req
                Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ =
        Just
            <$> envClientSessionBackend
                (7 * 24 * 60) -- timeout in minutes
                "CLIENT_SESSION_KEY"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware
    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        muser <- maybeAuthPair
        mcurrentRoute <- getCurrentRoute
        -- Define the menu items of the header.
        let menuItems =
                [ NavbarLeft $
                    MenuItem
                        { menuItemLabel = "Today"
                        , menuItemRoute = TodayR
                        , menuItemAccessCallback = isJust muser
                        }
                , NavbarLeft $
                    MenuItem
                        { menuItemLabel = "Manage"
                        , menuItemRoute = HomeR
                        , menuItemAccessCallback = isJust muser
                        }
                , NavbarLeft $
                    MenuItem
                        { menuItemLabel = "New Task"
                        , menuItemRoute = NewTaskR
                        , menuItemAccessCallback = isJust muser
                        }
                , NavbarRight $
                    MenuItem
                        { menuItemLabel = "Login with Google"
                        , menuItemRoute = AuthR $ oauth2Url "google"
                        , menuItemAccessCallback = isNothing muser
                        }
                , NavbarRight $
                    MenuItem
                        { menuItemLabel = "Logout"
                        , menuItemRoute = AuthR LogoutR
                        , menuItemAccessCallback = isJust muser
                        }
                ]
        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]
        let navbarLeftFilteredMenuItems = filter menuItemAccessCallback navbarLeftMenuItems
        let navbarRightFilteredMenuItems = filter menuItemAccessCallback navbarRightMenuItems
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
        let jsUserId = case muser of
                Nothing -> rawJS ("0" :: String)
                Just (userKey, _) -> rawJS $ show $ fromSqlKey userKey
        pc <-
            widgetToPageContent $ do
                addStylesheet $ StaticR css_bootstrap_css
                addStylesheet $ StaticR css_fontawesome_min_css
                $(widgetFile "update-tz")
                $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute :: App -> Maybe (Route App)
    authRoute _ = Just LandingR
    isAuthorized ::
        -- | The route the user is visiting.
        Route App ->
        -- | Whether or not this is a "write" request.
        Bool ->
        Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized LandingR _ = return Authorized
    isAuthorized HomeR _ = isAuthenticated
    isAuthorized NewTaskR _ = isAuthenticated
    isAuthorized (EditTaskR taskId) _ = userOwnsTask taskId
    isAuthorized (DeleteTaskR taskId) _ = userOwnsTask taskId
    isAuthorized (PinTaskR taskId) _ = userOwnsTask taskId
    isAuthorized (QuickPostponeR taskId) _ = userOwnsTask taskId
    isAuthorized (PostponeTaskR taskId) _ = userOwnsTask taskId
    isAuthorized (PostponeDateR taskId) _ = userOwnsTask taskId
    isAuthorized (UnpostponeR taskId) _ = userOwnsTask taskId
    isAuthorized TodayR _ = isAuthenticated
    isAuthorized (MarkDoneR taskId) _ = userOwnsTask taskId
    isAuthorized ResetDueTimeR _ = isAuthenticated
    isAuthorized TimeZoneR _ = isAuthenticated

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ::
        -- | The file extension
        Text ->
        -- | The MIME content type
        Text ->
        -- | The contents of the file
        LByteString ->
        Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself

        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
            appShouldLogAll (appSettings app)
                || level == LevelWarn
                || level == LevelError
    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

userOwnsTask :: Key Task -> Handler AuthResult
userOwnsTask taskId = do
    mu <- maybeAuthId
    case mu of
        Nothing -> return AuthenticationRequired
        Just userId -> do
            mtask <- runDB $ selectFirst [TaskId ==. taskId, TaskUserId ==. userId] []
            case mtask of
                Nothing -> return $ Unauthorized "This isn't your task."
                Just _ -> return Authorized

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

newtype GoogleUser = GoogleUser {googleUserEmail :: Text}
    deriving (Generic, Show)

instance FromJSON GoogleUser where
    parseJSON = withObject "GoogleUser" $ \v ->
        GoogleUser
            <$> v .: "email"

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = TodayR

    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = LandingR

    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authenticate ::
        (MonadHandler m, HandlerSite m ~ App) =>
        Creds App ->
        m (AuthenticationResult App)
    authenticate creds =
        liftHandler $
            runDB $ do
                let email = case googleUserEmail <$> getUserResponseJSON creds of
                        Right email' -> email'
                        Left _ -> credsIdent creds
                x <- getBy $ UniqueEmail email
                currTime <- liftIO getCurrentTime
                case x of
                    Just (Entity uid _) -> do
                        updateWhere [UserEmail ==. email] [UserLastLogin =. currTime]
                        return $ Authenticated uid
                    Nothing ->
                        Authenticated
                            <$> insert
                                User
                                    { userEmail = email
                                    , userFirstName = Nothing
                                    , userLastName = Nothing
                                    , userDueTime = Nothing
                                    , userDueTimeOffset = Nothing
                                    , userLastLogin = currTime
                                    , userCreateTime = currTime
                                    }

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins app = [oauth2Google (googleClientId app) (googleClientSecret app), authDummy]

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
