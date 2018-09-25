{-# LANGUAGE OverloadedStrings #-}
module Auth where

import           Data.ByteString                      (ByteString)
import           Data.Maybe
import           Data.Text                            (Text, pack)
import qualified Data.Vector                          as V
import           GitHub
import           GitHub.Endpoints.Organizations.Teams
import           GitHub.Endpoints.Users
import           GitHub.Request
import           Settings
import           Yesod.Auth.OAuth2.Prelude

newtype GithubUser = GithubUser String

instance FromJSON GithubUser where
    parseJSON = withObject "User" $ \o -> GithubUser <$> o .: "login"

findTeam :: GithubTeam ->  V.Vector SimpleTeam -> Maybe SimpleTeam
findTeam t = V.find (\apiteam -> simpleTeamName apiteam == team t)

apiTeam :: Maybe Auth -> GithubTeam -> IO (Either Error (Maybe SimpleTeam))
apiTeam auth team = do
  ets <- teamsOf' auth (mkOrganizationName $ org team)
  return $ fmap (findTeam team) ets

userBelongsToTeam :: Auth -> Text -> SimpleTeam -> IO (Either Error Bool)
userBelongsToTeam auth user team = do
  emembers <- executeRequest auth $ listTeamMembersR (simpleTeamId team) TeamMemberRoleAll FetchAll
  return $ do
    members <- fmap (V.map (pack . show . untagId . simpleUserId)) emembers
    return  $ elem user members

userBelongsToAnyTeam :: Auth -> Text -> [SimpleTeam] -> IO (Either Error Bool)
userBelongsToAnyTeam auth user teams = do
  ebs <- mapM (userBelongsToTeam auth user) teams
  case sequence ebs of
    (Right bs) -> return $ return $ or bs
    (Left err) -> return $ Left err

isTokenAuthorized :: Text -> [GithubTeam] -> ByteString -> IO (Either Error Bool)
isTokenAuthorized userId teams token = do
  apiTeams <- fmap catMaybes . sequence <$> mapM (apiTeam justAuth) teams
  case apiTeams of
    (Right ts) -> userBelongsToAnyTeam auth userId ts
    (Left err) -> return $ Left err
  where
    auth = OAuth token
    justAuth = Just auth
