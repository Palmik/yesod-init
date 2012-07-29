{-# LANGUAGE TemplateHaskell #-}

module Site.Controller.Handler.Profile.Update
( getProfileUpdateR
, postProfileUpdateR
) where

------------------------------------------------------------------------------
import           Control.Applicative
------------------------------------------------------------------------------
import qualified Yesod.Persist               as P
------------------------------------------------------------------------------
import           Site.Controller.Handler.Common
import           Site.Controller.Form.Profile
import           Site.Model.Type.Identity
------------------------------------------------------------------------------

getProfileUpdateR :: Handler RepHtml
getProfileUpdateR = do
    P.Entity iid (Identity mpid) <- requireIdentity
    maybeProfile' <- maybeProfile
    ((res, form), enctype) <- runFormPostNoToken $ update maybeProfile'
    case (res, mpid) of
         (FormSuccess profile, Just pid) -> P.runDB $ P.replace pid profile
         (FormSuccess profile, Nothing)  -> do
             P.runDB $ do 
                 pid <- P.insert profile
                 _   <- P.update iid [IdentityProfile P.=. Just pid]
                 return ()
                 
             redirect ProfileUpdateR
             
         _ -> return ()

    defaultLayout $(widgetFile "route/profile/update")

postProfileUpdateR :: Handler RepHtml
postProfileUpdateR = getProfileUpdateR