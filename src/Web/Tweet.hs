{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

-- | Various utilities to tweet using the twitter api
-- 
-- Make sure you have a file credentials file (default `.cred`) with the following info:
--
-- api-key: API_KEY
--
-- api-sec: API_SECRE
--
-- tok: OAUTH_TOKEN
--
-- tok-sec: TOKEN_SECRET

-- | Functions to tweet
{--module Web.Tweet
    -- * Functions to Tweet
    ( basicTweet
    , tweetData
    -- * Data types
    , Tweet
    , status
    , trimUser
    , handles
    , replyID
    -- * Functions to sign API requests
    , signRequest
    -- * Functions to generate a URL string from 
    , urlString
    ) where--}
module Web.Tweet where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import Web.Authenticate.OAuth
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (toLower)

-- | Data type for our request
data Tweet = Tweet
    { status   :: String
    , trimUser :: Bool
    , handles  :: [String]
    , replyID  :: Maybe Int
    } deriving Generic

instance ToJSON Tweet where

-- | Basic tweet, not a reply to anything
basicTweet :: BS.ByteString -> FilePath -> IO Int
basicTweet contents = tweetData (mkTweet contents)

-- | Make a tweet with only the contents
mkTweet :: BS.ByteString -> Tweet
mkTweet contents = Tweet { status = BS.unpack contents , trimUser = False, handles = [], replyID = Nothing }

-- | tweet given a Tweet
tweetData :: Tweet -> FilePath -> IO Int
tweetData tweet filepath = do
    let requestString = urlString tweet
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest ("https://api.twitter.com/1.1/statuses/update.json" ++ requestString)
    request <- signRequest filepath $ initialRequest { method = "POST" }
    response request manager

-- | print output of a request
response :: Request -> Manager -> IO Int
response request manager = do
    response <- httpLbs request manager
    putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
    BSL.putStrLn $ responseBody response
    return $ (read . (takeWhile (/=',')) . (drop 52)) (BSL.unpack $ responseBody response)

-- | Sign a request using your OAuth dev token.
-- Uses the IO monad because signatures require a timestamp
signRequest :: FilePath -> Request -> IO Request
signRequest filepath req = do
    o <- oAuth filepath
    c <- credential filepath
    signOAuth o c req

-- | Create an OAuth token
oAuth :: FilePath -> IO OAuth
oAuth filepath = do
    secret <- (lineByKey "api-sec") <$> getConfigData filepath
    key <- (lineByKey "api-key") <$> getConfigData filepath
    let url = "api.twitter.com"
    return newOAuth { oauthConsumerKey = key , oauthConsumerSecret = secret , oauthServerName = url }

-- | Create a new credential from a token and secret component of that token
credential :: FilePath -> IO Credential
credential filepath = newCredential <$> token <*> secretToken
    where token       = (lineByKey "tok") <$> getConfigData filepath
          secretToken = (lineByKey "tok-sec") <$> getConfigData filepath

-- | Pick out a key value from a key
lineByKey :: BS.ByteString -> [(BS.ByteString, BS.ByteString)] -> BS.ByteString
lineByKey key = snd . head . (filter (\i -> fst i == key))

-- | Get pairs of "key" to search for and actual values
getConfigData :: FilePath -> IO [(BS.ByteString, BS.ByteString)]
getConfigData filepath = zip <$> keys <*> content
    where content = (map (BS.pack . filterLine)) . lines <$> file
          keys    = (map (BS.pack . keyLinePie)) . lines <$> file
          file    = readFile filepath

-- | helper function to get the key as read from a file
keyLinePie :: String -> String
keyLinePie = takeWhile (/=':')

-- | Filter a line of a file for only the actual data and no descriptors
filterLine :: String -> String
filterLine = reverse . (takeWhile (not . (`elem` (" :" :: String)))) . reverse

-- | Convert a tweet to a percent-encoded url for querying an API
urlString :: Tweet -> String
urlString tweet = concat [ "?status="
                         , BS.unpack (tweetEncode tweet)
                         , "&trim_user= "
                         , map toLower (show trim)
                         , "&in_reply_to_status_id="
                         , show reply ]
    where trim  = trimUser tweet
          reply = maybe 0 id (replyID tweet)

tweetEncode :: Tweet -> BS.ByteString
tweetEncode tweet = paramEncode $ handleStr `BS.append` content
    where content   = BS.pack . status $ tweet
          handleStr = BS.pack $ concatMap ((++ " ") . ((++) "@")) hs
          hs        = handles tweet
