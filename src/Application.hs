{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Lostation
import Network.Wai.Middleware.Cors
import Yesod.Core

import Handler.Locks
import Handler.Pools

mkYesodDispatch "App" resourcesApp

makeApplication :: IO Application
makeApplication = do
  app <- toWaiApp App
  return $ simpleCors app
