{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Site.Core
( Environment(..)
, resourcesEnvironment

, Handler
, Widget
, Form

, Y.Route(..)
) where

------------------------------------------------------------------------------
import          Control.Applicative
------------------------------------------------------------------------------
import qualified Yesod                  as Y
import qualified Yesod.Auth             as Y
import qualified Yesod.Static           as Y
import qualified Yesod.Auth.OpenId      as Y
import qualified Yesod.Default.Config   as Y (AppConfig, DefaultEnv, appRoot, appExtra)
import qualified Yesod.Default.Util     as Y (addStaticContentExternal)
import qualified Yesod.Logger           as Y (Logger, logMsg, formatLogText)

import           Yesod.Auth                  (Auth)   -- We can not import this qualified, because of TH & QQ.
import           Yesod.Static                (Static) -- We can not import this qualified, because of TH & QQ.

import qualified Web.ClientSession      as Y (getKey)
------------------------------------------------------------------------------
import qualified Text.Jasmine as J (minifym)
import qualified Text.Hamlet  as Y (hamletFile)
------------------------------------------------------------------------------
import qualified Network.HTTP.Conduit as N (Manager)
------------------------------------------------------------------------------
import qualified Yesod.Persist               as P
import qualified Database.Persist.Store      as P
import qualified Database.Persist.GenericSql as P
------------------------------------------------------------------------------
import           Site.Settings
import           Site.Static
import           Site.Controller.Routes
import           Site.Model.Schema
------------------------------------------------------------------------------

-- | The environment of the application.
--
-- The reason the type is defined here is to avoid orphaned instances.
data Environment = Environment
    { envConfig :: Y.AppConfig Y.DefaultEnv Extra
    , envLogger :: Y.Logger
    , envDBConnection :: P.PersistConfigPool PersistConfig
    , envDBConfig :: PersistConfig
    , envHTTPManager :: N.Manager

    -- | Subsites
    , envSubStatic :: Y.Static
    , envSubAuth :: Y.Auth
    }

Y.mkYesodData "Environment" routes

type Form x = Y.Html -> Y.MForm Environment Environment (Y.FormResult x, Widget)

------------------------------------------------------------------------------
-- | Yesod class instance defining setting for the application.
instance Y.Yesod Environment where
    -- | An absolute URL to the root of the application without trailing slash.
    approot = Y.ApprootMaster $ Y.appRoot . envConfig

    -- | Setup the default layout.
    defaultLayout widget = do
        master <- Y.getYesod
        mmsg   <- Y.getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- Y.widgetToPageContent $ do
            Y.addStylesheet $ StaticR css_style_css
            $(widgetFile "default")
        Y.hamletToRepHtml $(Y.hamletFile "templates/default-wrapper.hamlet")

    -- | Enables sessions with 120 minute timeout.
    makeSessionBackend _ = do
        key <- Y.getKey "config/client_session_key.aes"
        return . Just $ Y.clientSessionBackend key 120

    -- | Sends a message to the log.
    messageLogger Environment{envLogger = logger} loc level msg =
        Y.formatLogText logger loc level msg >>= Y.logMsg logger

    -- | This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (Y.joinPath y (staticRoot $ envConfig y)) $ Y.renderRoute s
    urlRenderOverride _ _ = Nothing

    -- | The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR Y.LoginR

    -- | This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        Y.addStaticContentExternal J.minifym Y.base64md5 staticDir (StaticR . flip Y.StaticRoute [])

    -- | Place JavaScript at bottom of the body tag, so that the rest of the page loads first.
    jsLoader _ = Y.BottomOfBody

------------------------------------------------------------------------------
-- | YesodAuth class instance.
instance Y.YesodAuth Environment where
    type AuthId Environment = IdentityId

    getAuthId Y.Creds{Y.credsPlugin = method, Y.credsIdent = handle} = do
        miid1 <- Y.maybeAuthId
        miid2 <- P.runDB $ do
            mentity <- P.getBy $ UniqueCreds method handle
            case mentity of
                 Just (P.Entity _ (IdentityCreds _ _ iid)) -> return $! Just iid
                 Nothing -> return Nothing

        case (miid1, miid2) of
             -- The user is authenticated and these credentials are already associated with some account.
             (Just iid1, Just iid2) -> return $! Just iid2

             -- The user is authenticated and these credentials are not associated with any account.
             (Just iid1, Nothing) -> do
                 _ <- P.runDB $ P.insert $ IdentityCreds method handle iid1 -- Associate these credentials with this user.
                 return $! Just iid1
                 
             -- The user is not authenticated and these credenticals are associated with some account.
             (Nothing, Just iid2) -> return $! Just iid2

             -- The user is not autheticated and these credentials are not associated with any account.
             (Nothing, Nothing) -> P.runDB $ do
                 -- Create new identity and associate these credentials with it.
                 iid <- P.insert $ Identity Nothing
                 _   <- P.insert $ IdentityCreds method handle iid
                 return $! Just iid
                 

    -- | Where to send a user after successful login.
    loginDest _ = RootR
    
    -- | Where to send a user after logout.
    logoutDest _ = RootR

    -- | You can add other plugins like BrowserID, email or OpenID here
    authPlugins _ = [Y.authOpenId]

    authHttpManager = envHTTPManager

------------------------------------------------------------------------------
-- | YesodPersist class instance.
instance Y.YesodPersist Environment where
    type YesodPersistBackend Environment = P.SqlPersist
    runDB f =
        Y.getYesod >>= (\x -> P.runPool (envDBConfig x) f (envDBConnection x))

------------------------------------------------------------------------------
-- | YesodAuth class instance.
--
-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance Y.RenderMessage Environment Y.FormMessage where
    renderMessage _ _ = Y.defaultFormMessage
