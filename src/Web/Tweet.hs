{-# LANGUAGE OverloadedStrings #-}

-- | Various utilities to tweet using the twitter api
-- 
-- Make sure you have a file credentials file (default the executable looks for is `.cred`) with the following info:
--
-- @
--
-- api-key: API_KEY
--
-- api-sec: API_SECRE
--
-- tok: OAUTH_TOKEN
--
-- tok-sec: TOKEN_SECRET
--
-- @

module Web.Tweet
    (
    -- * Functions to tweet
    basicTweet
    , tweetData
    , thread
    -- * Data type for a tweet
    , module Web.Tweet.Types
    -- * Functions to sign API requests
    , signRequest
    -- * Functions to generate a URL string from a `Tweet`
    , urlString
    ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Char (toLower)
import Web.Tweet.Types
import Web.Tweet.Utils
import Control.Monad
import Data.List.Split (chunksOf)
import Data.Maybe
import Control.Lens
import Web.Authenticate.OAuth
import Web.Tweet.Sign

-- | thread tweets together nicely. Takes a string, a list of handles to reply to, plus the ID of the status you're replying to.
-- If you need to thread tweets without replying, pass a `Nothing` as the third argument.
--
-- > thread "Hi I'm back in New York!" ["friend1","friend2"] Nothing 1 ".cred"
thread :: String -> [String] -> Maybe Int -> Int -> FilePath -> IO ()
thread contents hs idNum num filepath = do
    let handleStr = concatMap (((++) " ") . ((++) "@")) hs
    let content = (take num) . (chunksOf (140-(length handleStr))) $ contents
    case idNum of
        (Just i) -> thread' content hs idNum num filepath
        Nothing -> case content of
            [] -> pure ()
            [x] -> void $ basicTweet x filepath
            (x:xs) -> do
                firstId <- basicTweet x filepath
                thread' xs hs (Just firstId) num filepath

-- | Helper function to make `thread` easier to write. 
thread' :: [String] -> [String] -> Maybe Int -> Int -> FilePath -> IO ()
thread' content hs idNum num filepath = do
    let f = (\str i -> (flip tweetData filepath) (Tweet { _status = str, _trimUser = True, _handles = hs, _replyID = if i == 0 then Nothing else Just i }))
    let initial = f (head content)
    void $ foldr ((>=>) . f) initial (drop 1 content) $ maybe 0 id idNum

-- | Reply with a single tweet. Works the same as `thread` but doesn't take the fourth argument.
--
-- > reply "Idk what that means" ["friend1"] (Just 189943500) ".cred"
reply :: String -> [String] -> Maybe Int -> FilePath -> IO ()
reply contents hs idNum = thread contents hs idNum 1

-- | Tweet a string given a path to credentials; return the id of the status.
--
-- > basicTweet "On the airplane." ".cred"
basicTweet :: String -> FilePath -> IO Int
basicTweet contents = tweetData (mkTweet contents)

-- | Make a `Tweet` with only the contents.
mkTweet :: String -> Tweet
mkTweet contents = over (status) (const (contents)) $ def 

-- | tweet, given a `Tweet` and path to credentials. Return id of posted tweet.
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

-- | Convert a tweet to a percent-encoded url for querying an API
urlString :: Tweet -> String
urlString tweet = concat [ "?status="
                         , BS.unpack (tweetEncode tweet)
                         , "&trim_user="
                         , map toLower (show trim)
                         , (if isJust (_replyID tweet) then "&in_reply_to_status_id=" else "")
                         , reply ]
    where trim  = _trimUser tweet
          reply = maybe "" id (fmap show $ _replyID $ tweet)

-- | Percent-ncode the status update so it's fit for a URL
tweetEncode :: Tweet -> BS.ByteString
tweetEncode tweet = paramEncode . encodeUtf8 $ handleStr `T.append` content
    where content   = T.pack . _status $ tweet
          handleStr = T.pack $ concatMap ((++ " ") . ((++) "@")) hs
          hs        = _handles tweet
