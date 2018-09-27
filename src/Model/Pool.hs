{-# LANGUAGE DeriveGeneric #-}
module Model.Pool where

import Data.Aeson
import Data.List
import GHC.Generics
import System.Directory

data Pool = Pool {poolName :: String, poolHasLifecycle :: Bool}
  deriving (Eq, Show, Ord, Generic)

instance ToJSON Pool
instance ToJSONKey Pool

listPools :: FilePath -> IO [Pool]
listPools d = do
  pools <- delete ".gitignore" .  delete ".git" <$> listDirectory d
  return $ makePools pools


makePools :: [String] -> [Pool]
makePools poolNames =
  let basePoolNames = filter (not . isInfixOf "lifecycle") poolNames
   in
   zipWith Pool basePoolNames $ map (\p -> (p ++ "-lifecycle") `elem` poolNames) basePoolNames
