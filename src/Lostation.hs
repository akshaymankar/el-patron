{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Lostation where

import Yesod.Core

data App = App

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App
