{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Model.LockOwner where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Attoparsec.Text as A
import Data.Text
import Settings

data LockOwner = Committer String
               | Pipeline { pipeline :: String, job :: String, buildNumber :: Int }
               | GafferUser String
  deriving (Show, Eq)

instance ToJSON LockOwner where
  toJSON (Committer c) = object [ "type" .= ("Committer" :: String)
                                , "committer" .= c ]
  toJSON Pipeline{..}  = object [ "type" .= ("Pipeline" :: String)
                                , "pipeline" .= pipeline
                                , "job" .= job
                                , "buildNumber" .= buildNumber ]
  toJSON (GafferUser u)  = object [ "type" .= ("GafferUser" :: String)
                                  , "username" .= u ]

commitAuthor :: String -> IO String
commitAuthor path = unpack <$> execGit ["log", "-1", "--pretty=%an", "--", pack path]

pipelineParser :: Parser LockOwner
pipelineParser = do
  p <- unpack <$> takeTill (\x -> x == '/')
  _ <- char '/'
  j <- unpack <$> takeTill (\x -> x == ' ')
  _ <- A.string " build "
  b <- decimal
  return $ Pipeline p j b

gafferUserParser :: Parser LockOwner
gafferUserParser = do
  username <- unpack <$> takeTill (\x -> x == ':')
  _ <- A.string ": Move" -- To verify that commit was made to move the lock
  return $ GafferUser username

parseOwnerFromCommitMessage :: Text -> Maybe LockOwner
parseOwnerFromCommitMessage m = parseToMaybe pipelineParser m <|> parseToMaybe gafferUserParser m

readLockOwner :: FilePath -> IO LockOwner
readLockOwner lockPath = do
  commitMessage <- execGit ["log", "-1", "--pretty=%s", "--", pack lockPath]
  case parseOwnerFromCommitMessage commitMessage of
    Just x -> return x
    Nothing -> Committer <$> commitAuthor lockPath

parseToMaybe :: Parser a -> Text -> Maybe a
parseToMaybe p t = maybeResult (parse p t)
