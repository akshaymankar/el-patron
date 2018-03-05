{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Model.Lock where

import Data.List as L
import Data.Map as M
import Data.Text (pack)
import Git.Types
import Git.CmdLine
import Model.Pool
import Shelly (shelly, errExit)
import Settings
import System.Directory
import Text.Blaze
import Data.Aeson

data Lock = Lock { name :: String, path :: String, state :: LockState }
data LockState = Claimed | Unclaimed | WaitingToRecycle | Recycling
  deriving Show

instance ToMarkup LockState where
  toMarkup x = toMarkup $ show x

instance ToJSON Lock where
  toJSON Lock{..} = object
    [ "name" .= name
    , "state" .= show state
    ]

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

-- TODO: Handle concurrent access to same git repo
moveLock :: (?locksPath :: FilePath) =>  Pool -> String -> Pool -> String -> String -> IO ()
moveLock pool lockName destinationPool from to =
  let commitMsg = pack $ "Move " ++ pool ++ "/" ++ from ++ "/" ++ lockName ++ " to " ++ destinationPool ++ "/" ++ to ++ "/" ++ lockName
      lockPath = ?locksPath ++ "/" ++ pool ++ "/" ++ from ++ "/" ++ lockName
      destinationPath = ?locksPath ++ "/" ++ destinationPool ++ "/" ++ to ++ "/" ++ lockName in do
      pathExists <- doesPathExist lockPath
      renameFile lockPath destinationPath
      _ <- execGit ["add", "-A", "."]
      _ <- execGit ["commit", "-m", commitMsg]
      _ <- execGit ["pull", "--rebase"]
      _ <- execGit ["push"]
      return ()

claim :: (?locksPath :: FilePath) => Pool -> String -> IO ()
claim pool lockName = moveLock pool lockName pool "unclaimed" "claimed"

unclaim :: (?locksPath :: FilePath) => Pool -> String -> IO ()
unclaim pool lockName = moveLock pool lockName pool "claimed" "unclaimed"

recycle :: (?locksPath :: FilePath) => Pool -> String -> IO ()
recycle pool lockName = moveLock pool lockName (pool ++ "-lifecycle" ) "claimed" "unclaimed"
