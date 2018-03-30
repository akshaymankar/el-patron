{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ImplicitParams #-}
module Handler.Locks where

import Data.Map
import Lostation
import Model.Lock
import Model.Pool
import Settings as S
import Yesod.Core

getLocksR :: Handler Value
getLocksR = let ?locksPath = S.locksPath in
                do
                  groupedLocks <- lift getAllLocks
                  returnJson $ groupedLocks

postClaimLockR :: Pool -> String -> Handler Value
postClaimLockR = doLockAction claim

postUnclaimLockR :: Pool -> String -> Handler Value
postUnclaimLockR = doLockAction unclaim

postRecycleLockR :: Pool -> String -> Handler Value
postRecycleLockR = doLockAction recycle

-- TODO: Handle absence of username better
doLockAction :: LockAction -> Pool -> String -> Handler Value
doLockAction action pool lock = do
  maybeUsername <- lookupSession "username"
  case maybeUsername of
    (Just username) -> do
      _ <- lift $ action S.locksPath username pool lock
      returnJson $ ([] :: [String])
    _ -> returnJson $ (["Error!!"] :: [String])
