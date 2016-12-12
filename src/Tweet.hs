{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

-- | Various utilities to tweet using the twitter api
module Tweet
    ( exec
    ) where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import Web.Authenticate.OAuth
import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)
import Data.List.Split (chunksOf)

-- | Data type for our request
data Tweet = Tweet
    { status    :: String
    , trim_user :: Bool
    } deriving Generic

instance ToJSON Tweet where

-- | query twitter to post stdin
exec :: IO ()
exec = do
    content <- fmap ((take 4) . (map BS.pack) .  (chunksOf 140)) getContents --aka stdin aka compatible w/ pipes
    sequence_ $ fmap tweet content

-- | tweet a byteString
tweet :: BS.ByteString -> IO ()
tweet content = do
    requestString <- urlString content
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest ("https://api.twitter.com/1.1/statuses/update.json" ++ requestString)
    request <- signRequest $ initialRequest { method = "POST" }
    response request manager

-- | print output of a request
response :: Request -> Manager -> IO ()
response request manager = do
    response <- httpLbs request manager
    putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
    print $ responseBody response

-- | Sign a request using your OAuth dev token.
-- Uses the IO monad because signatures require a timestamp
signRequest :: Request -> IO Request
signRequest req = do
    o <- oAuth
    c <- credential
    signOAuth o c req

-- | Create an OAuth token
oAuth :: IO OAuth
oAuth = do
    secret <- (flip (!!) 1) <$> getConfigData
    key <- (flip (!!) 0) <$> getConfigData
    let url = "api.twitter.com"
    return newOAuth { oauthConsumerKey = key , oauthConsumerSecret = secret , oauthServerName = url }

-- | Create a new credential from a token and secret component of that token
credential :: IO Credential
credential = newCredential <$> token <*> secretToken
    where token       = (flip (!!) 2) <$> getConfigData
          secretToken = (flip (!!) 3) <$> getConfigData

getConfigData :: IO [BS.ByteString]
getConfigData = ((map (BS.pack . filterLine)) . lines) <$> readFile ".cred"

-- | Filter a line of a file for only the actual data and no descriptors
filterLine :: String -> String
filterLine = reverse . (takeWhile (not . (`elem` (" :" :: String)))) . reverse

-- | Convert a byteString to the percent-encoded version
urlString :: BS.ByteString -> IO String
urlString content = do
    return $ "?status=" ++ ((BS.unpack . paramEncode) content) ++ "&trim_user" ++ (map toLower $ show True)
