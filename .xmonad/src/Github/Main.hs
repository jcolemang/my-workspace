
import Network.Wreq
import Data.Aeson
import Control.Lens


data PullRequest = PullRequest
  { title       :: String
  , updated     :: String
  , user        :: String
  , description :: String
  }

instance FromJSON PullRequest where
  parseJSON _ = error ""


getPullRequests :: (FromJSON a) => String -> IO (Maybe [a])
getPullRequests url =
    decode . (^. responseBody) <$> get url

main = error ""
