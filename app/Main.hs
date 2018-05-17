{-# LANGUAGE RecordWildCards #-}
import Application (makeApplication, cloneRepository) -- for YesodDispatch instance
import qualified Data.Attoparsec.Text as A
import Data.Semigroup ((<>))
import Data.Text (pack)
import Lostation
import Network.Wai.Handler.Warp (run)
import Options.Applicative
import Options.Applicative.Types
import Settings
import Yesod.Core

data Options = Options { remote :: String
                       , privateKeyFile :: String
                       , githubClientID :: String
                       , githubClientSecret :: String
                       , frontend :: String
                       , authorizedTeams :: [GithubTeam]
                       }
  deriving Show

makeSettings :: Options -> Settings
makeSettings Options{..} =
  Settings (pack remote)
           (pack privateKeyFile)
           (GithubOAuthKeys (pack githubClientID)
           (pack githubClientSecret))
           frontend
           authorizedTeams

attoReadM :: A.Parser a -> ReadM a
attoReadM p = eitherReader (A.parseOnly p . pack)

options :: Parser Options
options = Options
  <$> strOption
     ( long "remote"
     <> metavar "REPO"
     <> help "Git remote for the locks repository" )
  <*> strOption
     ( long "private-key"
     <> metavar "FILE"
     <> help "Private key for the git repository" )
  <*> strOption
     ( long "github-client-id"
     <> metavar "ID"
     <> help "Git client ID for OAuth" )
  <*> strOption
     ( long "github-client-secret"
     <> metavar "SECRET"
     <> help "Git client secret for OAuth" )
  <*> strOption
     ( long "frontend"
     <> help "Frontend to be allowed in Access-Control-Allow-Origin response header" )
  <*> (fromM $ someM $ option (attoReadM parseTeam)
        ( long "authorizedTeam"
        <> short 't'
        <> metavar "ORG/TEAM"
        <> help "list of authorized teams" ))


main :: IO ()
main = do
  gafferOpts <- execParser opts
  cloneRepository $ makeSettings gafferOpts
  app <- makeApplication $ makeSettings gafferOpts
  run 3000 app
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Runs El Patrón API service" )
