{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

module Site.Settings
( PersistConfig
, persistSettings

, staticDir
, staticRoot

, widgetFile

, Extra(..)
, parseExtra

, development
, production
) where

------------------------------------------------------------------------------
import           Language.Haskell.TH.Syntax
------------------------------------------------------------------------------
import           Control.Applicative
------------------------------------------------------------------------------
import qualified Data.Aeson       as AE
import qualified Data.Aeson.Types as AE (Parser)
import qualified Data.Text        as TS
------------------------------------------------------------------------------
import           Text.Shakespeare.Text (st) -- We can not import this qualified, because of TH & QQ.
------------------------------------------------------------------------------
import qualified Yesod.Default.Config as Y
import qualified Yesod.Default.Util   as Y
------------------------------------------------------------------------------
import qualified Database.Persist.TH         as P
import qualified Database.Persist.Postgresql as P
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Basic settings.

-- | Location of directory containing static files.
staticDir :: FilePath
staticDir = "static"

-- | Root URL from where you static files are to be served. 
staticRoot :: Y.AppConfig Y.DefaultEnv x -> TS.Text
staticRoot conf = [st|#{Y.appRoot conf}/static|]

------------------------------------------------------------------------------
-- | Database settings.

-- | Persistent backend used.
type PersistConfig = P.PostgresConf

-- | Persist settings.
persistSettings :: P.MkPersistSettings
persistSettings = P.sqlSettings

------------------------------------------------------------------------------
-- | Extra settings.

widgetFile :: String -> Q Exp
widgetFile = if development
                then Y.widgetFileReload
                else Y.widgetFileNoReload

data Extra = Extra
    { extraCopyright :: TS.Text
    , extraAnalytics :: Maybe TS.Text -- ^ Google Analytics
    } deriving Show

parseExtra :: Y.DefaultEnv -> AE.Object -> AE.Parser Extra
parseExtra _ o = Extra
    <$> o AE..:  "copyright"
    <*> o AE..:? "analytics"

------------------------------------------------------------------------------
-- | Convenience functions.
    
development :: Bool
development =
#if DEVELOPMENT
  True
#else
  False
#endif

production :: Bool
production = not development