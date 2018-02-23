{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Add where

import Lostation
import Yesod.Core

getAddR :: Int -> Int -> Handler TypedContent
getAddR x y = selectRep $ do
    provideRep $ defaultLayout $ do
        setTitle "Addition"
        [whamlet|#{x} + #{y} = #{z}|]
    provideJson $ object ["result" .= z]
  where
    z = x + y
