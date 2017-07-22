{-# LANGUAGE OverloadedStrings #-}

import Network.Wreq ( get
                    , responseBody )
import System.Environment ( getArgs )
import Data.Aeson ( FromJSON
                  , (.:)
                  , (.:?)
                  , Value ( Object )
                  , parseJSON
                  , decode )
import Control.Lens ( (^.) )
import Control.Monad ( join )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Trans.Maybe ( runMaybeT
                                 , MaybeT ( MaybeT ) )
import Data.Maybe ( catMaybes
                  , fromMaybe )


class PPrint a where
  pprint :: a -> String


data PullRequest = PullRequest
  { repo        :: String
  , title       :: String
  , updated     :: String
  , user        :: String
  , description :: String
  } deriving Show

instance FromJSON PullRequest where
  parseJSON (Object o) =
    PullRequest
    <$> (o .: "head" >>= (.: "repo") >>= (.: "name")) -- repo
    <*> (o .: "title") -- title
    <*> (o .: "updated_at") -- date updated
    <*> (o .: "user" >>= (.: "login")) -- user
    <*> (o .: "body")
    -- <*> fmap (fromMaybe "") (o .:? "body") -- description
  parseJSON _ =
    mempty

instance PPrint PullRequest where
  pprint pr =
    repo pr ++ ": " ++ title pr



--getPullRequests :: (FromJSON a) => String -> IO (Maybe a)
maybeGetPullRequests :: String -> MaybeT IO [PullRequest]
maybeGetPullRequests url = do
  resp <- liftIO . get $ url
  let mpr = decode . (^. responseBody) $ resp
    in MaybeT . return $ mpr

getAllPullRequests :: [String] -> IO [PullRequest]
getAllPullRequests urls =
  (join . catMaybes) <$> mapM (runMaybeT . maybeGetPullRequests) urls

main = do
  urls <- getArgs
  prs <- getAllPullRequests urls
  repr <- return . unlines $ pprint <$> prs
  putStr repr
