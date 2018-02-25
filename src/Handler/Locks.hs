{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ImplicitParams #-}
module Handler.Locks where

import Data.Map
import Lostation
import Model.Lock
import Model.Pool
import Yesod.Core

--locksPath = "/Users/axeman/work/kubo/kubo-locks"
locksPath = "/Users/axeman/work/test-locks"

getLocksR :: Handler Html
getLocksR = let ?locksPath = locksPath in
  defaultLayout $ do
    setTitle "Locks"
    groupedLocks <- lift getAllLocks
    $(whamletFile "whamlets/locks.whamlet")

postClaimLockR :: Pool -> String -> Handler Html
postClaimLockR pool lock = let ?locksPath = locksPath in
  defaultLayout $ do
  setTitle "move the file"
  _ <- lift $ claim pool lock
  [whamlet|<p>Locks moved|]
