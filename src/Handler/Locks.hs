{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ImplicitParams #-}
module Handler.Locks where

import Data.Map
import Lostation
import Model.Lock
import Model.Pool
import Settings
import Yesod.Core

getLocksR :: Handler Value
getLocksR = let ?locksPath = locksPath in
                do
                  groupedLocks <- lift getAllLocks
                  returnJson $ groupedLocks

postClaimLockR :: Pool -> String -> Handler Html
postClaimLockR pool lock = let ?locksPath = locksPath in
  defaultLayout $ do
  setTitle "move the file"
  _ <- lift $ claim pool lock
  [whamlet|<p>Locks moved|]

postUnclaimLockR :: Pool -> String -> Handler Html
postUnclaimLockR pool lock = let ?locksPath = locksPath in
  defaultLayout $ do
  setTitle "move the file"
  _ <- lift $ unclaim pool lock
  [whamlet|<p>Locks moved|]

postRecycleLockR :: Pool -> String -> Handler Html
postRecycleLockR pool lock = let ?locksPath = locksPath in
  defaultLayout $ do
  setTitle "move the file"
  _ <- lift $ recycle pool lock
  [whamlet|<p>Locks moved|]
