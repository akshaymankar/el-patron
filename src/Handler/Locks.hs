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

postClaimLockR :: Pool -> String -> Handler Value
postClaimLockR pool lock = let ?locksPath = locksPath in
  do
  _ <- lift $ claim pool lock
  returnJson $ ([] :: [String])

postUnclaimLockR :: Pool -> String -> Handler Value
postUnclaimLockR pool lock = let ?locksPath = locksPath in
  do
  _ <- lift $ unclaim pool lock
  returnJson $ ([] :: [String])

postRecycleLockR :: Pool -> String -> Handler Value
postRecycleLockR pool lock = let ?locksPath = locksPath in
  do
  _ <- lift $ recycle pool lock
  returnJson $ ([] :: [String])
