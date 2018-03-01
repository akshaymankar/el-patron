{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Lostation where

import Data.Text (pack)
import Git.Types
import Settings
import System.Directory
import Yesod.Core

data App = App

mkYesodData "App" $(parseRoutesFile "routes")

cloneRepository :: IO ()
cloneRepository =  do
  tmpDirExists <- doesPathExist tmpDir
  if tmpDirExists
     then (removeDirectoryRecursive tmpDir)
     else (print $ tmpDir ++ " doesn't exist")
  execGit ["clone", "--separate-git-dir", pack gitDir, locksRepoRemote, pack locksRepoFile]

instance Yesod App
