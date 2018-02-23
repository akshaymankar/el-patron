{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Lostation
import Yesod.Core

import Handler.Add
import Handler.Home
import Handler.Locks
import Handler.Pools

mkYesodDispatch "App" resourcesApp
