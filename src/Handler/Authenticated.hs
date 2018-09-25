module Handler.Authenticated where

import Lostation
import Yesod.Core

getAuthenticatedR :: Handler Html
getAuthenticatedR = redirect "/"

