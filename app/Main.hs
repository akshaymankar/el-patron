import Application () -- for YesodDispatch instance
import Lostation
import Yesod.Core

main :: IO ()
main = do
  cloneRepository
  warp 3000 App
