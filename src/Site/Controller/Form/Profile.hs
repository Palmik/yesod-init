{-# LANGUAGE OverloadedStrings #-}

module Site.Controller.Form.Profile
( update
, updateA
) where

------------------------------------------------------------------------------
import           Control.Applicative
------------------------------------------------------------------------------
import qualified Data.Text as TS (Text)
------------------------------------------------------------------------------
import qualified Yesod.Form as Y
------------------------------------------------------------------------------
import           Site.Core            (Environment, Form)
import           Site.Model.Type.Profile
------------------------------------------------------------------------------

update :: Maybe Profile -> Form Profile
update = Y.renderDivs . updateA

updateA :: Maybe Profile -> Y.AForm Environment Environment Profile
updateA mprofile = Profile
    <$> Y.areq nameField "Name" (profileName <$> mprofile)
    <*> Y.areq ageField  "Age"  (profileAge  <$> mprofile)
    where
        nameField = Y.textField
        ageField  = Y.checkBool (>= 0) ("You must enter a positive number as your age." :: TS.Text) Y.intField
          