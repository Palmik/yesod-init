{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE EmptyDataDecls   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Site.Model.Schema
( migrateAll
  -- * Profile
, ProfileGeneric(..)
, ProfileId
, Profile

  -- * Identity
, IdentityGeneric(..)
, IdentityId
, Identity

  -- * IdentityCreds
, IdentityCredsGeneric(..)
, IdentityCredsId
, IdentityCreds

, Unique(..)
, EntityField(..)
) where

------------------------------------------------------------------------------
import           Yesod
import           Data.Text (Text)
import           Database.Persist.Quasi
------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Profile    
    name  Text
    age   Int
    deriving Show

Identity
    profile ProfileId Maybe

IdentityCreds
    method Text -- ^ Corresponds to credsPlugin
    handle Text -- ^ Corresponds to credsIdent
    
    identity IdentityId

    UniqueCreds method handle
|]