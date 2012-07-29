{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TypeFamilies    #-}

module Site.Controller.Routes
( routes
) where

------------------------------------------------------------------------------
import           Yesod.Core      (renderRoute) -- We can not import this qualified, because of the limitations of the parseRoutes QQ.
import           Yesod.Dispatch  (parseRoutes) -- We can not import this qualified, because of TH & QQ.
import           Yesod.Routes.TH
------------------------------------------------------------------------------

routes :: [Resource String]
routes = [parseRoutes|

/static StaticR Static envSubStatic
/auth   AuthR   Auth   envSubAuth

/                  RootR             GET

/profile/update    ProfileUpdateR    GET POST

|]
