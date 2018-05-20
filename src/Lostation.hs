{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lostation where

import Auth
import Data.Aeson as JSON
import Data.ByteString hiding (pack)
import qualified Data.ByteString.Lazy as BL
import Data.Map
import Data.Text (pack, Text)
import Data.Text.Encoding
import Git.Types
import Network.HTTP.Client.Conduit (Manager, newManager)
import Settings (GithubOAuthKeys, clientID, clientSecret, GithubTeam)
import Yesod.Auth
import Yesod.Auth.Message
import Yesod.Auth.OAuth2.Github
import Yesod.Core
import Yesod.Form


data App = App { httpManager :: Manager
               , githubOAuthKeys :: GithubOAuthKeys
               , frontendUrl :: String
               , backendUrl :: String
               , authorizedTeams :: [GithubTeam]
               }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
  authRoute _ = Just $ AuthR LoginR
  isAuthorized LocksR _ = isAuthorizedForLocks
  isAuthorized (ClaimLockR _ _) _ = isAuthorizedForLocks
  isAuthorized (UnclaimLockR _ _) _ = isAuthorizedForLocks
  isAuthorized (RecycleLockR _ _) _ = isAuthorizedForLocks
  isAuthorized AuthenticatedR _ = return Authorized
  isAuthorized (AuthR _) _ = return Authorized
  isAuthorized _ _ = return $ Unauthorized "because why not"
  approot = ApprootMaster $ pack . backendUrl

isAuthorizedForLocks :: HandlerT App IO AuthResult
isAuthorizedForLocks = do
  maybeUserId <- maybeAuthId
  case maybeUserId of
    Nothing -> return $ Unauthorized "User not logged in."
    _ -> return $ Authorized

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

accessToken :: Creds master -> ByteString
accessToken c = (encodeUtf8 $ (fromList (credsExtra c)) ! "accessToken")

persistAuthInfo :: Creds master -> GithubUser -> HandlerT App IO ()
persistAuthInfo c (GithubUser username) = do
  _ <- mapM_ (uncurry setSession) $ credsExtra c
  _ <- setSession "userId" (credsIdent c)
  _ <- setSession "username" (pack username)
  return ()

encodeToLazyBS :: Text -> BL.ByteString
encodeToLazyBS = BL.fromStrict . encodeUtf8

instance YesodAuth App where
  type AuthId App = Text
  getAuthId = return . Just . credsIdent

  loginDest _ = AuthenticatedR
  logoutDest _ = AuthenticatedR

  authPlugins m = [oauth2GithubScoped ["read:org"]
                                      (clientID $ githubOAuthKeys m)
                                      (clientSecret $ githubOAuthKeys m)]

  authHttpManager = httpManager

  maybeAuthId = lookupSession "_ID"

  authenticate c = do
    app <- getYesod
    eAuthorized <- lift $ isTokenAuthorized (credsIdent c) (authorizedTeams app) (accessToken c)
    case eAuthorized of
      (Right True) -> do
        case JSON.decode . encodeToLazyBS $ extrasMap ! "userResponse" of
          (Just x) -> do
            persistAuthInfo c x
            return $ Authenticated (credsIdent c)
          Nothing -> return $ UserError AuthError
        where
          extrasMap = fromList $ credsExtra c
      _ -> return $ UserError AuthError
