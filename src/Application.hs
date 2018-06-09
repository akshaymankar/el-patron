{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Lostation
import Network.HTTP.Client.Conduit (newManager)
import Network.Wai.Middleware.Cors
import Network.Wai
import Yesod.Core
import Yesod.Auth
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.Text as T
import Settings as S
import System.Directory

import Handler.Locks
import Handler.Authenticated

mkYesodDispatch "App" resourcesApp

corsMiddleware :: ByteString -> Middleware
corsMiddleware frontend =
  cors (const $ Just CorsResourcePolicy { corsOrigins = Just ([frontend], True)
                                        , corsMethods = simpleMethods
                                        , corsRequestHeaders = []
                                        , corsExposedHeaders = Nothing
                                        , corsMaxAge = Nothing
                                        , corsVaryOrigin = False
                                        , corsRequireOrigin = False
                                        , corsIgnoreFailures = False
                                        })

makeApplication :: Settings -> IO Application
makeApplication s = do
  man <- newManager
  app <- toWaiApp $ App man (S.githubOAuthKeys s) (frontend s) (backend s) (S.authorizedTeams s)
  return $ corsMiddleware (pack $ frontend s) app

cloneRepository :: Settings -> IO ()
cloneRepository settings =  do
  tmpDirExists <- doesPathExist tmpDir
  if tmpDirExists
     then (removeDirectoryRecursive tmpDir)
     else (print $ tmpDir ++ " doesn't exist")
  _ <- execGit [ "clone"
               , "-c", "core.sshCommand" `T.append` "=ssh -i " `T.append` privateKeyFile settings
               , "--separate-git-dir", T.pack gitDir
               , lockRepoRemote settings, T.pack locksRepoFile]
  return ()
