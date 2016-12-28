{-# LANGUAGE OverloadedStrings #-}

-- | Various utilities to tweet using the twitter api
-- 
-- Make sure you have a file credentials file (default `.cred`) with the following info:
--
-- ```
--
-- api-key: API_KEY
--
-- api-sec: API_SECRE
--
-- tok: OAUTH_TOKEN
--
-- tok-sec: TOKEN_SECRET
--
-- ```

-- | Functions to tweet
module Web.Tweet
    -- * Functions to Tweet
    ( basicTweet
    , tweetData
    , thread
    -- * Data types
    , module Web.Tweet.Types
    -- * Functions to sign API requests
    , signRequest
    -- * Functions to generate a URL string from 
    , urlString
    ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import Web.Authenticate.OAuth
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (toLower)
import Web.Tweet.Types
import Web.Tweet.Utils
import Control.Monad
import Data.List.Split (chunksOf)

-- | thread tweets together nicely. Takes a list of handles to reply to, plus the ID of the status you're replying to.
thread :: [String] -> Int -> Int -> FilePath -> IO ()
thread hs replyID num filepath = do
    let handleStr = concatMap (((++) " ") . ((++) "@")) hs
    content <- (take num) . (chunksOf (140-(length handleStr))) <$> getContents
    print $ urlString (Tweet { status = content !! 0, trimUser = True, handles = hs, replyID = pure 0})
    let f = (\str i -> (flip tweetData filepath) (Tweet { status = str, trimUser = True, handles = hs, replyID = Just i }))
    let initial = f (content !! 0)
    void $ foldr ((>=>) . f) initial content $ replyID

-- | Basic tweet, not a reply to anything
basicTweet :: BS.ByteString -> FilePath -> IO Int
basicTweet contents = tweetData (mkTweet contents)

-- | Make a tweet with only the contents
mkTweet :: BS.ByteString -> Tweet
mkTweet contents = Tweet { status = BS.unpack contents , trimUser = False, handles = [], replyID = Nothing }

-- | tweet, given a Tweet. Return id of posted tweet.
tweetData :: Tweet -> FilePath -> IO Int
tweetData tweet filepath = do
    let requestString = urlString tweet
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest ("https://api.twitter.com/1.1/statuses/update.json" ++ requestString)
    request <- signRequest filepath $ initialRequest { method = "POST" }
    response request manager

-- | print output of a request and return status id as an `Int`. 
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

-- | Convert a tweet to a percent-encoded url for querying an API
urlString :: Tweet -> String
urlString tweet = concat [ "?status="
                         , BS.unpack (tweetEncode tweet)
                         , "&trim_user="
                         , map toLower (show trim)
                         , "&in_reply_to_status_id="
                         , show reply ]
    where trim  = trimUser tweet
          reply = maybe 0 id (replyID tweet)

-- | Encode the status update so it's fit for a URL
tweetEncode :: Tweet -> BS.ByteString
tweetEncode tweet = paramEncode $ handleStr `BS.append` content
    where content   = BS.pack . status $ tweet
          handleStr = BS.pack $ concatMap ((++ " ") . ((++) "@")) hs
          hs        = handles tweet
