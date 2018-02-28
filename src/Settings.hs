{-# LANGUAGE OverloadedStrings #-}
module Settings where

import Data.Text
import Git.Types
import Git.CmdLine
import Shelly
--locksPath = "/Users/axeman/work/kubo/kubo-locks"
locksPath = "/tmp/test-locks"

repoOptions = defaultRepositoryOptions { repoPath = locksPath ++ "/.git",
                                         repoWorkingDir = Just locksPath}

execGit :: [Text] -> IO ()
execGit args = do
  repo <- openCliRepository repoOptions
  _ <- shelly $ errExit True $ git repo args
  return ()
