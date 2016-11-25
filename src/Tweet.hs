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
import Data.ByteString.Char8 (pack, unpack)
import Data.Char (toLower)

data Tweet = Tweet
    { status    :: String
    , trim_user :: Bool
    } deriving Generic

instance ToJSON Tweet where

--query twitter to post stdin (aka what's passed in via pipes)
exec :: IO ()
exec = do
    requestString <- urlString
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest ("https://api.twitter.com/1.1/statuses/update.json" ++ requestString)
    request <- signRequest $ initialRequest { method = "POST" }
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

urlString :: IO String
urlString = do
    content <- fmap (pack . (take 140)) getContents --aka stdin aka compatible w/ pipes
    return $ "?status=" ++ ((unpack . paramEncode) content) ++ "&trim_user" ++ (map toLower $ show True)
