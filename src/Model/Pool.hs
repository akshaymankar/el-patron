module Model.Pool where

import Data.List
import System.Directory

type Pool = String

listPools :: FilePath -> IO [Pool]
listPools d = do
  pools <- (delete ".gitignore") <$> (delete ".git") <$> listDirectory d
  return $ filter (not.(isInfixOf "lifecycle")) pools
