{-# LANGUAGE CPP                   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Site
( makeApplication
, getApplicationDev
) where

------------------------------------------------------------------------------
import           Control.Applicative
------------------------------------------------------------------------------
import qualified Yesod                as Y (Application)
import qualified Yesod.Auth           as Y (getAuth)
import qualified Yesod.Core                (yesodDispatch)
import qualified Yesod.Logger         as Y (Logger, logBS, toProduction)
import qualified Yesod.Dispatch       as Y (mkYesodDispatch, toWaiAppPlain)
import qualified Yesod.Default.Config as Y (AppConfig, DefaultEnv(..), ConfigSettings(..), appEnv, withYamlEnvironment, loadConfig, configSettings)
import qualified Yesod.Default.Main   as Y (defaultDevelApp)
------------------------------------------------------------------------------
import qualified Network.Wai.Middleware.RequestLogger as W
import qualified Network.HTTP.Conduit                 as H
------------------------------------------------------------------------------
import qualified Database.Persist.Store      as P
import qualified Database.Persist.GenericSql as P
------------------------------------------------------------------------------
import           Site.Core
import           Site.Settings
import           Site.Static
import           Site.Model.Schema
------------------------------------------------------------------------------
import           Site.Controller.Handler.Root
import           Site.Controller.Handler.Profile.Update
------------------------------------------------------------------------------

Y.mkYesodDispatch "Environment" resourcesEnvironment

makeApplication :: Y.AppConfig Y.DefaultEnv Extra -> Y.Logger -> IO Y.Application
makeApplication config logger = logWare <$> (makeCore config setLogger >>= Y.toWaiAppPlain)
  where
    setLogger = if development
                   then logger
                   else Y.toProduction logger
    logWare   = if development
                   then W.logCallbackDev (Y.logBS setLogger)
                   else W.logCallback    (Y.logBS setLogger)

makeCore :: Y.AppConfig Y.DefaultEnv Extra -> Y.Logger -> IO Environment
makeCore config logger = do
    manager <- H.newManager H.def
    static <- staticSite
    dbconf <- Y.withYamlEnvironment "config/postgresql.yml" (Y.appEnv config)
              P.loadConfig >>= P.applyEnv
    dbconn <- P.createPoolConfig (dbconf :: PersistConfig)
    P.runPool dbconf (P.runMigration migrateAll) dbconn
    return $ Environment
         { envConfig = config
         , envLogger = logger
         , envDBConnection = dbconn
         , envDBConfig = dbconf
         , envHTTPManager = manager

         , envSubStatic = static
         , envSubAuth = Y.getAuth (0 :: Int)
         }

-- for yesod devel
getApplicationDev :: IO (Int, Y.Application)
getApplicationDev =
    Y.defaultDevelApp loader makeApplication
  where
    loader = Y.loadConfig (Y.configSettings Y.Development)
        { Y.csParseExtra = parseExtra
        }