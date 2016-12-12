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
signRequest = signOAuth oAuth credential

-- | Create an OAuth token
oAuth :: OAuth
oAuth = newOAuth { oauthConsumerKey = key , oauthConsumerSecret = secret , oauthServerName = url }
    where secret = "CzUrfL4lbhdY7qMyGPDq6rVOuNglkTp7fTKEvRfr2P1iIMd16a"
          key    = "375wBiEuvaxHMjE7OAiDKSveH"
          url    = "api.twitter.com"

-- | Create a new credential from a token and secret component of that token
credential :: Credential
credential = newCredential token secretToken
    where token       = "802268569585287168-mL9GTRozVUQRH3g7yLr0TQ2vx3AFuiY"
          secretToken = "P0a2vMaNvBJaismFjeQSzUccpetB1LzeNql9OsNcFhj92"

-- | Convert a byteString to the percent-encoded version
urlString :: BS.ByteString -> IO String
urlString content = do
    return $ "?status=" ++ ((BS.unpack . paramEncode) content) ++ "&trim_user" ++ (map toLower $ show True)
