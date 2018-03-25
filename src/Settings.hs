{-# LANGUAGE OverloadedStrings #-}
module Settings where

import Data.Text
import Git.Types
import Git.CmdLine
import Shelly
import Data.Attoparsec.Text as A
--locksPath = "/Users/axeman/work/kubo/kubo-locks"
tmpDir = "/tmp/gaffer"
locksPath = tmpDir ++ "/locks"
gitDir = tmpDir ++ "/git"
locksRepoFile = tmpDir ++ "/locks-repo-file"

data GithubOAuthKeys = GithubOAuthKeys {clientID :: Text, clientSecret :: Text}

data GithubTeam = GithubTeam { org :: Text, team :: Text }
  deriving Show

parseTeam :: Parser GithubTeam
parseTeam = do
  o <- A.takeWhile1 (\x -> x /= '/')
  _ <- A.skip (\x -> x == '/')
  t <- A.takeText
  return $ GithubTeam o t


teamFromString :: String -> Maybe GithubTeam
teamFromString = undefined


data Settings = Settings { lockRepoRemote :: Text
                         , githubOAuthKeys :: GithubOAuthKeys
                         , frontend :: String
                         , authroizedTeams :: [GithubTeam] }

repoOptions = defaultRepositoryOptions { repoPath = gitDir,
                                         repoWorkingDir = Just $ locksPath }

execGit :: [Text] -> IO ()
execGit args = do
  repo <- openCliRepository repoOptions
  _ <- shelly $ errExit True $ git repo args
  return ()
