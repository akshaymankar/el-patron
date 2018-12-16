{-# LANGUAGE OverloadedStrings #-}
module Handler.Config where

import Data.Aeson
import Yesod.Core
import Lostation

getConfigR :: Handler Value
getConfigR = do
  app <- getYesod
  return $ object [ "disableActionButtons" .= disableActionButtons app ]
