{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lostation where

import Data.Text (pack, Text)
import Git.Types
import Network.HTTP.Client.Conduit (Manager, newManager)
import Settings (GithubOAuthKeys, clientID, clientSecret)
import Yesod.Auth
import Yesod.Auth.OAuth2.Github
import Yesod.Core
import Yesod.Form


data App = App { httpManager :: Manager
               , githubOAuthKeys :: GithubOAuthKeys
               }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
  authRoute _ = Just $ AuthR LoginR
  isAuthorized LocksR _ = isAuthorizedForLocks
  isAuthorized (AuthR _) _ = return Authorized
  isAuthorized _ _ = return $ Unauthorized "because why not"
  approot = ApprootStatic "http://localhost:3000"

isAuthorizedForLocks :: HandlerT App IO AuthResult
isAuthorizedForLocks = do
  maybeToken <- maybeAuthId
  return $ case maybeToken of
             Nothing -> Unauthorized "Locked out!"
             (Just token) -> Authorized

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAuth App where
  type AuthId App = Text
  getAuthId = return . Just . credsIdent

  loginDest _ = LocksR
  logoutDest _ = LocksR

  authPlugins m = [oauth2Github (clientID $ githubOAuthKeys m) (clientSecret $ githubOAuthKeys m)]

  authHttpManager = httpManager

  maybeAuthId = lookupSession "_ID"

  authenticate c = do
    _ <- mapM_ (uncurry setSession) $ credsExtra c
    return $ Authenticated "foo"
