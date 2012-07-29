{-# LANGUAGE TemplateHaskell #-}

module Site.Static
where

------------------------------------------------------------------------------
import           Yesod.Static -- We can not import this qualified, because of TH & QQ.
------------------------------------------------------------------------------
import           Site.Settings (staticDir, development)
------------------------------------------------------------------------------

staticSite :: IO Static
staticSite = if development then staticDevel staticDir
                            else static      staticDir

-- | This generates easy references to files in the static directory at compile time,
--   giving you compile-time verification that referenced files exist.
$(staticFiles staticDir)