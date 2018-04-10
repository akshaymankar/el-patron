{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Model.Lock where

import qualified Control.Concurrent.ParallelIO.Local as P
import Data.Aeson
import Data.Attoparsec.Text as A
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Text (pack, unpack, Text)
import Data.Time
import Data.Time.ISO8601
import Git.CmdLine
import Model.Pool
import Shelly (shelly, errExit)
import Settings
import System.Directory
import Text.Blaze

data Lock = Lock { name :: String, path :: String, state :: LockState, lockedSince :: UTCTime, owner :: LockOwner }
data LockState = Claimed | Unclaimed | WaitingToRecycle | Recycling
  deriving Show

data LockOwner = Committer String | Pipeline { pipeline :: String, job :: String, buildNumber :: Int }

instance ToJSON LockOwner where
  toJSON (Committer c) = object [ "type" .= ("Committer" :: String)
                                , "committer" .= c ]
  toJSON Pipeline{..}  = object [ "type" .= ("Pipeline" :: String)
                                , "pipeline" .= pipeline
                                , "job" .= job
                                , "buildNumber" .= buildNumber ]

instance ToMarkup LockState where
  toMarkup x = toMarkup $ show x

instance ToJSON Lock where
  toJSON Lock{..} = object [ "name" .= name
                           , "state" .= show state
                           , "lockedSince" .= formatISO8601 lockedSince
                           , "owner" .= owner
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

commitAuthor :: String -> IO String
commitAuthor path = unpack <$> execGit ["log", "-1", "--pretty=%an", "--", pack path]

commitParser :: Parser LockOwner
commitParser = do
  p <- unpack <$> takeTill (\x -> x == '/')
  _ <- char '/'
  j <- unpack <$> takeTill (\x -> x == ' ')
  _ <- A.string " build "
  b <- decimal
  return $ Pipeline p j b

readLockOwner :: FilePath -> IO LockOwner
readLockOwner lockPath = do
  commitMessage <- execGit ["log", "-1", "--pretty=%s", "--", pack lockPath]
  case parseOnly commitParser commitMessage of
    Right x -> return x
    Left _ -> Committer <$> commitAuthor lockPath

readLockFromFile :: FilePath -> LockState -> String -> IO Lock
readLockFromFile dir state name = do
  lockedSince <- authorTime lockPath
  owner <- readLockOwner lockPath
  return $ Lock name lockPath state lockedSince owner where
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
