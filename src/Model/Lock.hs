{-# LANGUAGE ImplicitParams #-}
module Model.Lock where

import Data.List as L
import Data.Map as M
import Model.Pool
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

claim :: (?locksPath :: FilePath) => Pool -> String -> IO ()
claim pool lockName = do
  let lockPath = ?locksPath ++ "/" ++ pool ++ "/unclaimed/" ++ lockName
      claimedPath = ?locksPath ++ "/" ++ pool ++ "/claimed/" ++ lockName in do
      pathExists <- doesPathExist lockPath
      if pathExists then
         renameFile lockPath claimedPath
      else error "Lock not found in unclaimed"
