import Application (makeApplication, cloneRepository) -- for YesodDispatch instance
import Data.Semigroup ((<>))
import Data.Text (pack)
import Lostation
import Network.Wai.Handler.Warp (run)
import Options.Applicative
import Settings
import Yesod.Core

data Options = Options { remote :: String
                       , githubClientID :: String
                       , githubClientSecret :: String
                       , frontend :: String}
  deriving Show

makeSettings :: Options -> Settings
makeSettings (Options repo gId gSecret frontend) = Settings (pack repo) (GithubOAuthKeys (pack gId) (pack gSecret)) frontend

options :: Parser Options
options = Options
  <$> strOption
     ( long "remote"
     <> metavar "REPO"
     <> help "Git remote for the locks repository" )
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


main :: IO ()
main = do
  gafferOpts <- execParser opts
  cloneRepository $ makeSettings gafferOpts
  app <- makeApplication $ makeSettings gafferOpts
  run 3000 app
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Runs gaffer backend service" )
