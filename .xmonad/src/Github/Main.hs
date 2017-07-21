{-# LANGUAGE OverloadedStrings #-}

import Network.Wreq
import Data.Aeson
import Control.Lens
import Data.ByteString
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe


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
    <*> (o .: "body") -- description
  parseJSON _ =
    mempty


--getPullRequests :: (FromJSON a) => String -> IO (Maybe a)
getPullRequests :: String -> MaybeT IO [PullRequest]
getPullRequests url = do
  resp <- liftIO . get $ url
  let mpr = decode . (^. responseBody) $ resp
    in MaybeT . return $ case mpr of
                           Nothing -> Nothing
                           Just pr -> Just pr

main = runMaybeT $ do
  prs <- getPullRequests "https://api.github.com/repos/haskell/mtl/pulls"
  liftIO . print $ prs



  -- print $  >>= (\m -> return $ m >>= (decode :: ByteString -> PullRequest))
