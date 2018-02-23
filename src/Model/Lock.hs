module Model.Lock where

import Data.List as L
import Data.Map as M
import Model.Pool
import System.Directory

data Lock = Lock { name :: String, path :: String }
data LockState = Claimed | Unclaimed | WaitingToRecycle | Recycling

getAllLocks :: FilePath -> IO (Map Pool [Lock])
getAllLocks locksPath = do
  pools <- listPools locksPath
  lockss <- sequence $ fmap (readLocks locksPath) pools
  return $ fromList (zip pools lockss)

readLocks :: FilePath -> Pool -> IO [Lock]
readLocks locksPath pool = do
  claimedLocks <- readLocksFromDir (locksPath ++ "/" ++ pool ++ "/claimed")
  unclaimedLocks <- readLocksFromDir (locksPath ++ "/" ++ pool ++ "/unclaimed")
  recyclingLocks <- readLocksFromDir (locksPath ++ "/" ++ pool ++ "-lifecycle/claimed")
  tobeRecycledLocks <- readLocksFromDir (locksPath ++ "/" ++ pool ++ "-lifecycle/unclaimed")
  return $ claimedLocks ++ unclaimedLocks ++ recyclingLocks ++ tobeRecycledLocks

readLocksFromDir :: FilePath -> IO [Lock]
readLocksFromDir dir = do
  pathExists <- doesPathExist dir
  if pathExists then do
    names <- (L.delete ".gitkeep") <$> listDirectory dir
    return $ L.map (\name -> Lock name (dir ++ name)) names
  else
    return []

