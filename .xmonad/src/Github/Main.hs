{-# LANGUAGE OverloadedStrings #-}

import Network.Wreq ( get
                    , responseBody )
import System.Environment ( getArgs )
import Data.Aeson ( FromJSON
                  , (.:)
                  , Value ( Object )
                  , parseJSON
                  , decode )
import Control.Lens ( (^.) )
import Control.Monad ( join
                     , guard )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Trans.Maybe ( runMaybeT
                                 , MaybeT ( MaybeT ) )
import Data.Maybe ( catMaybes )
import Data.List ( elemIndex
                 , isInfixOf )
import Data.Yaml ( decodeFile )


class PPrint a where
  pprint :: a -> String


data Config =
  Config [String]
  deriving Show

instance FromJSON Config where
  parseJSON (Object o) =
    Config <$> o .: "repos"
  parseJSON _ =
    mempty


data PullRequest = PullRequest
  { _repo        :: String
  , _title       :: String
  , _updated     :: String
  , _user        :: String
  , _description :: String
  } deriving Show

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


getRepoUrl :: String -> String
getRepoUrl x =
  if "https://" `isInfixOf`  x
  then x
  else case elemIndex ':' x of
         Nothing -> x
         Just i ->
           let (user, repo) = splitAt i x
           in "https://api.github.com/repos/" ++ user ++ "/" ++ drop 1 repo ++ "/pulls"



maybeGetPullRequests :: String -> MaybeT IO [PullRequest]
maybeGetPullRequests url = do
  resp <- liftIO . get $ url
  let mpr = decode . (^. responseBody) $ resp
    in MaybeT . return $ mpr

getAllPullRequests :: [String] -> IO [PullRequest]
getAllPullRequests urls =
  (join . catMaybes) <$> mapM (runMaybeT . maybeGetPullRequests) urls

main :: IO String
main = do
  args <- getArgs
  guard $ length args == 1
  mconfig <- decodeFile $ head args
  (Config urls) <- maybe (fail "Could not read config file") return mconfig
  prs <- getAllPullRequests $ getRepoUrl <$> urls
  return . unlines $ pprint <$> prs
