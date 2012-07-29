{-# LANGUAGE TemplateHaskell #-}

module Site.Controller.Handler.Root
( getRootR
) where

------------------------------------------------------------------------------
import           Site.Controller.Handler.Common
import           Site.Model.Type.Profile
------------------------------------------------------------------------------

getRootR :: Handler RepHtml
getRootR = do
    hasIdentity'  <- hasIdentity
    maybeProfile' <- maybeProfile
    defaultLayout $(widgetFile "route/root")