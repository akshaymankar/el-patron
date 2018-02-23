{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Locks where

import Lostation
import Model.Lock
import Yesod.Core
import Data.Map

getLocksR :: Handler Html
getLocksR = defaultLayout $ do
    setTitle "Locks"
    groupedLocks <- lift $ getAllLocks "/Users/axeman/work/kubo/kubo-locks"
    $(whamletFile "whamlets/locks.whamlet")
