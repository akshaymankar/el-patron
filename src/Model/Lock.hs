{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.Lock where

import Data.List as L
import Data.Map as M
import Data.Text (pack)
import Git.Types
import Git.CmdLine
import Model.Pool
import Shelly (shelly)
import System.Directory
import Text.Blaze

data Lock = Lock { name :: String, path :: String, state :: LockState }
data LockState = Claimed | Unclaimed | WaitingToRecycle | Recycling
  deriving Show

instance ToMarkup LockState where
  toMarkup x = toMarkup $ show x

getAllLocks :: (?locksPath :: FilePath) => IO (Map Pool [Lock])
getAllLocks = do
  pools <- listPools ?locksPath
  lockss <- sequence $ fmap readLocks pools
  return $ fromList (zip pools lockss)

readLocks :: (?locksPath :: FilePath) => Pool -> IO [Lock]
readLocks pool = do
  claimedLocks      <- readLocksFromDir (?locksPath ++ "/" ++ pool ++ "/claimed") Claimed
  unclaimedLocks    <- readLocksFromDir (?locksPath ++ "/" ++ pool ++ "/unclaimed") Unclaimed
  recyclingLocks    <- readLocksFromDir (?locksPath ++ "/" ++ pool ++ "-lifecycle/claimed") Recycling
  tobeRecycledLocks <- readLocksFromDir (?locksPath ++ "/" ++ pool ++ "-lifecycle/unclaimed") WaitingToRecycle
  return $ claimedLocks ++ unclaimedLocks ++ recyclingLocks ++ tobeRecycledLocks

readLocksFromDir :: FilePath -> LockState -> IO [Lock]
readLocksFromDir dir state = do
  pathExists <- doesPathExist dir
  if pathExists then do
    names <- (L.delete ".gitkeep") <$> listDirectory dir
    return $ L.map (\name -> Lock name (dir ++ name) state) names
  else
    return []

moveLock :: (?locksPath :: FilePath) =>  Pool -> String -> Pool -> String -> String -> IO ()
moveLock pool lockName destinationPool from to =
  let repoOptions = defaultRepositoryOptions {repoPath = ?locksPath ++ "/.git",
                                              repoWorkingDir = Just ?locksPath}
      commitMsg = pack $ "Move " ++ pool ++ "/" ++ from ++ "/" ++ lockName ++ " to " ++ destinationPool ++ "/" ++ to ++ "/" ++ lockName
      lockPath = ?locksPath ++ "/" ++ pool ++ "/" ++ from ++ "/" ++ lockName
      destinationPath = ?locksPath ++ "/" ++ destinationPool ++ "/" ++ to ++ "/" ++ lockName in do
      pathExists <- doesPathExist lockPath
      renameFile lockPath destinationPath
      repo <- openCliRepository repoOptions
      _ <- shelly $ git repo [ "add", "-A", "."]
      _ <- shelly $ git repo [ "commit", "-m", commitMsg]
      return ()

claim :: (?locksPath :: FilePath) => Pool -> String -> IO ()
claim pool lockName = moveLock pool lockName pool "unclaimed" "claimed"

unclaim :: (?locksPath :: FilePath) => Pool -> String -> IO ()
unclaim pool lockName = moveLock pool lockName pool "claimed" "unclaimed"

recycle :: (?locksPath :: FilePath) => Pool -> String -> IO ()
recycle pool lockName = moveLock pool lockName (pool ++ "-lifecycle" ) "claimed" "unclaimed"
