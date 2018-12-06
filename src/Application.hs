{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import           Data.ByteString               (ByteString)
import           Data.ByteString.Char8         (pack)
import qualified Data.Text                     as T
import           Lostation
import           Network.HTTP.Client.Conduit   (newManager)
import           Network.Wai
import           Network.Wai.Middleware.Static
import           Settings                      as S
import           System.Directory
import           Yesod.Auth
import           Yesod.Core

import           Handler.Authenticated
import           Handler.Locks

mkYesodDispatch "App" resourcesApp

elmMiddleWare path = staticPolicy (policy (mapRootToIndexHtml path) <|> addBase path)

mapRootToIndexHtml :: String -> String -> Maybe String
mapRootToIndexHtml path "" = Just (path ++ "/index.html")
mapRootToIndexHtml _ p     = Nothing

makeApplication :: Settings -> IO Application
makeApplication s = do
  man <- newManager
  app <- toWaiApp $ App man (S.githubOAuthKeys s) (S.authorizedTeams s)
  return $ elmMiddleWare (compiledElmFiles s) app

cloneRepository :: Settings -> IO ()
cloneRepository settings =  do
  -- tmpDirExists <- doesPathExist tmpDir
  -- if tmpDirExists
  --    then removeDirectoryRecursive tmpDir
  --    else print $ tmpDir ++ " doesn't exist"
  -- _ <- execGit [ "clone"
  --              , "-c", "core.sshCommand" `T.append` "=ssh -i " `T.append` privateKeyFile settings
  --              , "--separate-git-dir", T.pack gitDir
  --              , lockRepoRemote settings, T.pack locksRepoFile]
  return ()
