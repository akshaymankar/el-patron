{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Pools where

import Lostation
import Yesod.Core
import Model.Pool

getPoolsR :: Handler Html
getPoolsR = defaultLayout $ do
  setTitle "Pools"
  pools <- lift $ listPools "/Users/axeman/work/kubo/kubo-locks"
  lift $ print pools
  $(whamletFile "whamlets/pools.whamlet")

