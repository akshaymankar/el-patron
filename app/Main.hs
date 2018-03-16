import Application (makeApplication) -- for YesodDispatch instance
import Data.Semigroup ((<>))
import Data.Text (pack)
import Lostation
import Network.Wai.Handler.Warp (run)
import Options.Applicative
import Settings
import Yesod.Core

data Options = Options { remote :: String }
  deriving Show

makeSettings :: Options -> Settings
makeSettings (Options repo) = Settings $ pack repo

options :: Parser Options
options = Options
  <$> strOption
     ( long "remote"
     <> metavar "REPO"
     <> help "Git remote for the locks repository" )

main :: IO ()
main = do
  gafferOpts <- execParser opts
  cloneRepository $ makeSettings gafferOpts
  app <- makeApplication
  run 3000 app
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Runs gaffer backend service" )
