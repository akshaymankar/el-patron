{-# LANGUAGE OverloadedStrings #-}
module Settings where

import Data.Text
import Git.Types
import Git.CmdLine
import Shelly
--locksPath = "/Users/axeman/work/kubo/kubo-locks"
tmpDir = "/tmp/gaffer"
locksPath = tmpDir ++ "/locks"
gitDir = tmpDir ++ "/git"
locksRepoFile = tmpDir ++ "/locks-repo-file"

data GithubOAuthKeys = GithubOAuthKeys {clientID :: Text, clientSecret :: Text}

data Settings = Settings { lockRepoRemote :: Text
                         , githubOAuthKeys :: GithubOAuthKeys
                         , frontend :: String}

repoOptions = defaultRepositoryOptions { repoPath = gitDir,
                                         repoWorkingDir = Just $ locksPath }

execGit :: [Text] -> IO ()
execGit args = do
  repo <- openCliRepository repoOptions
  _ <- shelly $ errExit True $ git repo args
  return ()
