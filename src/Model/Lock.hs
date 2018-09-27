{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Model.Lock where

import qualified Control.Concurrent.ParallelIO.Local as P
import           Data.Aeson
import           Data.Attoparsec.Text                as A
import           Data.List                           as L
import           Data.Map                            as M
import           Data.Map.Lazy                       as LazyMap
import           Data.Maybe
import           Data.Text                           (Text, pack, unpack)
import           Data.Time
import           Data.Time.ISO8601
import           Git.CmdLine
import           Model.LockOwner
import           Model.Pool
import           Settings
import           Shelly                              (errExit, shelly)
import           System.Directory

data Lock = Lock { name :: String, path :: String, state :: LockState }
data LockState = Claimed { owner :: LockOwner, claimedSince :: UTCTime }
               | Unclaimed
               | WaitingToRecycle { waitingSince :: UTCTime }
               | Recycling { recyclingSince :: UTCTime }
  deriving Show

data LockActionRequest = LockActionRequest { locksPath       :: FilePath
                                           , username        :: Text
                                           , sourcePool      :: String
                                           , destinationPool :: String
                                           , lockName        :: String
                                           , from            :: String
                                           , to              :: String
                                           }

instance ToJSON Lock where
  toJSON Lock{..} = object [ "name" .= name
                           , "state" .= toJSON state
                           ]


instance ToJSON LockState where
  toJSON Claimed{..} = object [ "name" .= ("Claimed" :: String)
                              , "owner" .= owner
                              , "since" .= claimedSince
                              ]
  toJSON Unclaimed = object [ "name" .= ("Unclaimed" :: String)]
  toJSON WaitingToRecycle{..} = object [ "name" .= ("WaitingToRecycle" :: String)
                                       , "since" .= waitingSince
                                       ]
  toJSON Recycling{..} = object [ "name" .= ("Recycling" :: String)
                                , "since" .= recyclingSince]

getAllLocks :: FilePath -> IO (Map Pool [Lock])
getAllLocks locksPath = do
  _ <- execGit ["pull", "--rebase"]
  pools <- listPools locksPath
  lockss <- traverse (readLocks locksPath) pools
  return $ LazyMap.filter (not.Prelude.null) $ fromList (zip pools lockss)

type StateCalculator = FilePath -> IO LockState

claimedStateCalculator :: StateCalculator
claimedStateCalculator locksPath             = Claimed <$> readLockOwner locksPath <*> authorTime locksPath
unclaimedStateCalculator locksPath           = return Unclaimed
waitingToRecycleStateCalculator locksPath = WaitingToRecycle <$> authorTime locksPath
recyclingStateCalculator locksPath           = Recycling <$> authorTime locksPath


readLocks :: FilePath -> Pool -> IO [Lock]
readLocks locksPath (Pool pool _) = do
  claimedLocks      <- readLocksFromDir (locksPath ++ "/" ++ pool ++ "/claimed") claimedStateCalculator
  unclaimedLocks    <- readLocksFromDir (locksPath ++ "/" ++ pool ++ "/unclaimed") unclaimedStateCalculator
  recyclingLocks    <- readLocksFromDir (locksPath ++ "/" ++ pool ++ "-lifecycle/claimed") recyclingStateCalculator
  tobeRecycledLocks <- readLocksFromDir (locksPath ++ "/" ++ pool ++ "-lifecycle/unclaimed") waitingToRecycleStateCalculator
  return $ claimedLocks ++ unclaimedLocks ++ recyclingLocks ++ tobeRecycledLocks

authorTime :: String -> IO UTCTime
authorTime path = (fromMaybe (error "Author time not parsable") . parseISO8601) . unpack
                  <$> execGit ["log", "-1", "--pretty=%aI", "--", pack path]

runIn8Threads :: [IO a] -> IO [a]
runIn8Threads x = P.withPool 8 $ \p ->  P.parallel p x

makeLock :: FilePath -> StateCalculator -> String -> IO Lock
makeLock dir stateCalc name =
  Lock name lockPath <$> stateCalc lockPath where
    lockPath = dir ++ "/" ++ name

readLocksFromDir :: FilePath -> StateCalculator -> IO [Lock]
readLocksFromDir dir stateCalc = do
  pathExists <- doesPathExist dir
  if pathExists then do
    names <- L.delete ".gitkeep" <$> listDirectory dir
    runIn8Threads $ L.map (makeLock dir stateCalc) names
  else
    return []

-- TODO: Handle concurrent access to same git repo
moveLock :: LockActionRequest -> IO ()
moveLock LockActionRequest{..} =
  let commitMsg = pack $ unpack username ++ ": Move " ++ sourcePool ++ "/" ++ from ++ "/" ++ lockName ++ " to " ++ destinationPool ++ "/" ++ to ++ "/" ++ lockName
      lockPath = locksPath ++ "/" ++ sourcePool ++ "/" ++ from ++ "/" ++ lockName
      destinationPath = locksPath ++ "/" ++ destinationPool ++ "/" ++ to ++ "/" ++ lockName in do
      pathExists <- doesPathExist lockPath
      renameFile lockPath destinationPath
      _ <- execGit ["add", "-A", "."]
      _ <- execGit ["commit", "-m", commitMsg]
      _ <- execGit ["pull", "--rebase"]
      _ <- execGit ["push"]
      return ()

type LockAction = FilePath -> Text -> String -> String -> IO ()

claim :: LockAction
claim locksPath username pool lockName = moveLock $ LockActionRequest { from = "unclaimed"
                                                                      , to = "claimed"
                                                                      , username = username
                                                                      , locksPath = locksPath
                                                                      , sourcePool = pool
                                                                      , lockName = lockName
                                                                      , destinationPool = pool}

unclaim :: LockAction
unclaim locksPath username pool lockName = moveLock $ LockActionRequest { from = "claimed"
                                                                        , to = "unclaimed"
                                                                        , username = username
                                                                        , locksPath = locksPath
                                                                        , sourcePool = pool
                                                                        , lockName = lockName
                                                                        , destinationPool = pool}

recycle :: LockAction
recycle locksPath username pool lockName = moveLock $ LockActionRequest { from = "claimed"
                                                                        , to = "unclaimed"
                                                                        , username = username
                                                                        , locksPath = locksPath
                                                                        , sourcePool = pool
                                                                        , lockName = lockName
                                                                        , destinationPool = pool ++ "-lifecycle"}
