{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Model.Lock where

import qualified Control.Concurrent.ParallelIO.Local as P
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Text (pack, unpack, Text)
import Data.Time
import Data.Time.ISO8601
import Git.Types
import Git.CmdLine
import Model.Pool
import Shelly (shelly, errExit)
import Settings
import System.Directory
import Text.Blaze
import Data.Aeson

data Lock = Lock { name :: String, path :: String, state :: LockState, lockedSince :: UTCTime }
data LockState = Claimed | Unclaimed | WaitingToRecycle | Recycling
  deriving Show

instance ToMarkup LockState where
  toMarkup x = toMarkup $ show x

instance ToJSON Lock where
  toJSON Lock{..} = object
    [ "name" .= name
    , "state" .= show state
    , "lockedSince" .= formatISO8601 lockedSince
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

authorTime :: String -> IO UTCTime
authorTime path = do
  fromMaybe undefined
                  <$> parseISO8601
                  <$> unpack
                  <$> execGit ["log", "-1", "--pretty=%aI", "--", pack path]

readLockFromFile :: FilePath -> LockState -> String -> IO Lock
readLockFromFile dir state name = Lock name lockPath state <$> authorTime lockPath where
  lockPath = (dir ++ "/" ++ name)

runIn8Threads :: [IO a] -> IO [a]
runIn8Threads x = P.withPool 8 $ \p ->  P.parallel p x

readLocksFromDir :: FilePath -> LockState -> IO [Lock]
readLocksFromDir dir state = do
  pathExists <- doesPathExist dir
  if pathExists then do
    names <- (L.delete ".gitkeep") <$> listDirectory dir
    runIn8Threads $ L.map (readLockFromFile dir state) names
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
