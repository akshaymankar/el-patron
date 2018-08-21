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
                       , authorizedTeams :: [GithubTeam]
                       , compiledElmFiles :: String
                       , port :: Int
                       }
  deriving Show

makeSettings :: Options -> Settings
makeSettings Options{..} =
  Settings (pack remote)
           (pack privateKeyFile)
           (GithubOAuthKeys (pack githubClientID)
           (pack githubClientSecret))
           authorizedTeams
           compiledElmFiles

attoReadM :: A.Parser a -> ReadM a
attoReadM p = eitherReader (A.parseOnly p . pack)

{-# ANN options ("HLint: ignore" :: String) #-}
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
  <*> (fromM $ someM $ option (attoReadM parseTeam)
        ( long "authorized-team"
        <> short 't'
        <> metavar "ORG/TEAM"
        <> help "list of authorized teams" ))
  <*> strOption
     ( long "compiled-elm-files"
     <> value "/elm"
     <> showDefault
     <> short 'e'
     <> metavar "PATH"
     <> help "Path to built elm sources" )
  <*> option auto
     ( long "port"
     <> value 3000
     <> showDefault
     <> short 'p'
     <> help "Port to run El Patrón" )

main :: IO ()
main = do
  opts <- execParser optsParser
  cloneRepository $ makeSettings opts
  app <- makeApplication $ makeSettings opts
  run (port opts) app
  where
    optsParser = info (options <**> helper)
      ( fullDesc
     <> progDesc "Runs El Patrón API service" )
