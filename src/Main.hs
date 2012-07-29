module Main
( main
) where

------------------------------------------------------------------------------
import           Control.Concurrent (threadDelay, forkIO)
------------------------------------------------------------------------------
import           System.Directory (doesFileExist, removeFile)
import           System.Exit (exitSuccess)
------------------------------------------------------------------------------
import qualified Yesod.Default.Config as Y (fromArgs)
import qualified Yesod.Default.Main   as Y (defaultMain)
------------------------------------------------------------------------------
import qualified Network.Wai.Handler.Warp as W (runSettings, defaultSettings, settingsPort)
------------------------------------------------------------------------------
import           Site
import           Site.Settings
------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "Starting devel application"
    (port, app) <- getApplicationDev
    forkIO $ W.runSettings W.defaultSettings
        { W.settingsPort = port
        } app
    loop

loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "dist/devel-terminate"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess

{-
main :: IO ()
main = Y.defaultMain (Y.fromArgs parseExtra) makeApplication
-}
