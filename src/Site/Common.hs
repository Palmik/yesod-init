module Site.Common
( maybeIdentity
, maybeIdentityId
, hasIdentity
, requireIdentity
 
, maybeProfile
, maybeProfileId
, hasProfile

, Handler
, Widget
, Form

, Y.FormResult(..)
, Y.Route(..)
, Y.redirect
) where

------------------------------------------------------------------------------
import           Control.Applicative
------------------------------------------------------------------------------
import           Data.Maybe
------------------------------------------------------------------------------
import qualified Yesod      as Y
import qualified Yesod.Auth as Y
------------------------------------------------------------------------------
import qualified Yesod.Persist               as P
import qualified Database.Persist.Store      as P
import qualified Database.Persist.GenericSql as P
------------------------------------------------------------------------------
import           Site.Core
import           Site.Model.Type.Profile
import           Site.Model.Type.Identity
------------------------------------------------------------------------------

maybeIdentity :: Handler (Maybe (P.Entity Identity))
maybeIdentity = Y.maybeAuth

maybeIdentityId :: Handler (Maybe IdentityId)
maybeIdentityId = Y.maybeAuthId

requireIdentity :: Handler (P.Entity Identity)
requireIdentity = Y.requireAuth

hasIdentity :: Handler Bool
hasIdentity = isJust <$> maybeIdentityId

maybeProfile :: Handler (Maybe Profile)
maybeProfile = do
    mident <- maybeIdentity
    case mident of
         Just (P.Entity _ (Identity (Just pid))) -> P.runDB $ P.get pid
         _ -> return Nothing

maybeProfileId :: Handler (Maybe ProfileId)
maybeProfileId = do
    mident <- maybeIdentity
    case mident of
         Just (P.Entity _ (Identity (Just pid))) -> return $! Just pid
         _ -> return Nothing

hasProfile :: Handler Bool
hasProfile = isJust <$> maybeProfileId