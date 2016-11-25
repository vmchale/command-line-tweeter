{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Tweet
    ( exec
    ) where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import Web.Authenticate.OAuth
import qualified Data.ByteString.Lazy.Char8 as BS

data Tweet = Tweet
    { status    :: String
    , trim_user :: Bool
    } deriving Generic

instance ToJSON Tweet where

--query twitter to post stdin (aka what's passed in via pipes)
exec :: IO ()
exec = do
    requestObject <- inputToTweet
    manager <- newManager tlsManagerSettings
    signedRequest <- parseRequest "https://api.twitter.com/1.1/statuses/update.json" >>= signRequest
    let request = signedRequest { method = "POST" , requestBody = RequestBodyLBS $ encode requestObject }
    print request
    response request manager

response :: Request -> Manager -> IO ()
response request manager = do
    response <- httpLbs request manager
    putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
    print $ responseBody response
    
signRequest :: Request -> IO Request --uses IO b/c signing requires a timestamp
signRequest = signOAuth oAuth credential

oAuth :: OAuth
oAuth = newOAuth { oauthConsumerKey = key , oauthConsumerSecret = secret , oauthServerName = url }
    where secret = "EwozUJbFXzXKjoYWdf9zYrxiSagz1eDbEMWLlfjzinn0RGEQrl"
          key    = "Zi4fdD888qYBYSD7n0QkRv6Yr"
          url    = "api.twitter.com"

credential :: Credential
credential = newCredential token secretToken
    where token       = "739626641450635265-O9qWyuHbglnmCablfw46D95VMnHp10P"
          secretToken = "mfvMsChCt6VbMCD8XVOjCK7bGbit2BmBvC1YujL3mRzxF"

inputToTweet :: IO Value
inputToTweet = do
    content <- fmap (take 140) getContents --aka stdin aka compatible w/ pipes
    return $ toJSON Tweet { status = content, trim_user = True }
