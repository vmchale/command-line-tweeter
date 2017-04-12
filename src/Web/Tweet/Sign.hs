{-# LANGUAGE OverloadedStrings #-}

-- | Functions to sign HTTP requests with oAuth
module Web.Tweet.Sign where

import Web.Tweet.Utils
import Web.Authenticate.OAuth
import Network.HTTP.Client
import Web.Tweet.Types

-- | Sign a request using your OAuth dev token, as stored in a config file.
-- Uses the IO monad because signatures require a timestamp
signRequest :: FilePath -> Request -> IO Request
signRequest = (. flip signRequestMem) . (>>=) . mkConfig

-- | Sign a request using a 'Config' object, avoiding the need to read token/key from file
signRequestMem :: Config -> Request -> IO Request
signRequestMem = uncurry signOAuth

-- | Create an OAuth api key from config data in a file
oAuth :: FilePath -> IO OAuth
oAuth filepath = do
    secret <- (lineByKey "api-sec") <$> getConfigData filepath
    key <- (lineByKey "api-key") <$> getConfigData filepath
    let url = "api.twitter.com"
    return newOAuth { oauthConsumerKey = key , oauthConsumerSecret = secret , oauthServerName = url }

mkConfig :: FilePath -> IO Config
mkConfig filepath = do
    o <- oAuth filepath
    c <- credential filepath
    pure (o, c)

-- | Create a new credential from a token and token secret
credential :: FilePath -> IO Credential
credential filepath = newCredential <$> token <*> secretToken
    where token       = (lineByKey "tok") <$> getConfigData filepath
          secretToken = (lineByKey "tok-sec") <$> getConfigData filepath
