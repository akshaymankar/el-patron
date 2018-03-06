import Application (makeApplication) -- for YesodDispatch instance
import Lostation
import Yesod.Core
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  cloneRepository
  app <- makeApplication
  run 3000 app
