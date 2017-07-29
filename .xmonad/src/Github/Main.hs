{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Network.Wreq ( get
                    , responseBody )
import System.Environment ( getArgs )
import Data.Aeson ( FromJSON
                  , (.:)
                  , Value ( Object
                          , Array )
                  , parseJSON
                  , decode )
import Control.Lens ( (^.) )
import Control.Monad ( join
                     , mzero
                     , guard )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Trans.Maybe ( runMaybeT
                                 , MaybeT ( MaybeT ) )
import Control.Applicative ( (<|>) )
import Data.Maybe ( catMaybes )
import Data.Yaml ( decodeFile )
import Data.Vector ( toList )


class PPrint a where
  pprint :: a -> String


newtype Username = User String
                   deriving Show

newtype Repo = Repo String
               deriving Show

data Config
  = Config [Url]
  deriving Show

data PullRequest
  = PullRequest
  { _repo        :: String
  , _title       :: String
  , _updated     :: String
  , _user        :: String
  , _description :: String
  } deriving Show

data Url
  = FullUrl String
  | Repository Username Repo
  deriving Show


instance FromJSON Config where
  parseJSON (Object o) =
    Config
    <$> (o .: "repos"
         >>=
         (\case
             Array v -> let val = mapM parseJSON $ toList v
                        in val
             _ -> mzero))
  parseJSON _ =
    mzero

instance FromJSON Url where
  parseJSON (Object o) = do
    repo <- o .: "repo"
    let fullUrl   = FullUrl <$> repo .: "url"
    let githubUrl = Repository
                    <$> (User <$> repo .: "username")
                    <*> (Repo <$> repo .: "repository")
    githubUrl <|> fullUrl
  parseJSON _ =
    mzero

instance FromJSON PullRequest where
  parseJSON (Object o) =
    PullRequest
    <$> (o .: "head" >>= (.: "repo") >>= (.: "name")) -- repo
    <*> (o .: "title") -- title
    <*> (o .: "updated_at") -- date updated
    <*> (o .: "user" >>= (.: "login")) -- user
    <*> (o .: "body")
  parseJSON _ =
    mempty

instance PPrint PullRequest where
  pprint pr =
    _repo pr ++ ": " ++ _title pr



getRepoUrl :: Url -> String
getRepoUrl x =
  case x of
    FullUrl url ->
      url
    Repository (User uname) (Repo repo) ->
      "https://api.github.com/repos/" ++ uname ++ "/" ++ repo ++ "/pulls"


maybeGetPullRequests :: String -> MaybeT IO [PullRequest]
maybeGetPullRequests url = do
  resp <- liftIO . get $ url
  let mpr = decode . (^. responseBody) $ resp
    in MaybeT . return $ mpr

getAllPullRequests :: [String] -> IO [PullRequest]
getAllPullRequests urls =
  (join . catMaybes) <$> mapM (runMaybeT . maybeGetPullRequests) urls

main :: IO ()
main = do
  args <- getArgs
  guard $ length args == 1
  mconfig <- decodeFile $ head args
  (Config urls) <- maybe (fail "Could not read config file") return mconfig
  prs <- getAllPullRequests $ getRepoUrl <$> urls
  putStr . unlines $ pprint <$> prs
