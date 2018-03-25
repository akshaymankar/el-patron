{-# LANGUAGE RecordWildCards #-}
module Handler.Authenticated where

import Lostation
import Yesod.Core

getAuthenticatedR :: Handler Html
getAuthenticatedR = do
  App {..} <- getYesod
  redirect frontendUrl

