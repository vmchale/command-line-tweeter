{-# LANGUAGE OverloadedStrings #-}

module Web.Tweet.API where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text.Encoding
import Web.Tweet.Types
import Web.Tweet.Utils
import Control.Monad
import Control.Lens
import Web.Tweet.Sign
import Text.Megaparsec.Error
import Web.Tweet.Utils.API

-- | Get tweets (text only) for some user
getAll :: String -> Maybe Int -> FilePath -> IO [String]
getAll screenName maxId filepath = do
    tweets <- either (error "Parse tweets failed") id <$> getProfileMax screenName 200 filepath maxId
    let lastId = _tweetId . last $ tweets
    if (Just lastId) == maxId then 
        pure []
    else
        do
            putStrLn $ "fetching tweets since " ++ show lastId ++ "..."
            next <- getAll screenName (Just lastId) filepath
            pure ((map _text tweets) ++ next)

-- | tweet, given a `Tweet` and path to credentials. Return id of posted tweet.
tweetData :: Tweet -> FilePath -> IO Int
tweetData tweet filepath = do
    let requestString = urlString tweet
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest ("https://api.twitter.com/1.1/statuses/update.json" ++ requestString)
    request <- signRequest filepath $ initialRequest { method = "POST" }
    responseInt request manager

-- | Gets user profile with max_id set.
getProfileMax :: String -> Int -> FilePath -> Maybe Int -> IO (Either (ParseError Char Dec) Timeline) -- TODO return bytes raw; parse elsewhere
getProfileMax screenName count filepath maxId = do
    let requestString = case maxId of {
        (Just id) -> "?screen_name=" ++ screenName ++ "&count=" ++ (show count) ++ "&max_id=" ++ (show id) ;
        Nothing -> "?screen_name=" ++ screenName ++ "&count=" ++ (show count) }
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest ("https://api.twitter.com/1.1/statuses/user_timeline.json" ++ requestString)
    request <- signRequest filepath $ initialRequest { method = "GET"}
    responseBS request manager -- TODO
    getTweets . BSL.unpack <$> responseBS request manager

-- | Get user profile given screen name and how many tweets to return
getProfile :: String -> Int -> FilePath -> IO (Either (ParseError Char Dec) Timeline)
getProfile screenName count filepath = getProfileMax screenName count filepath Nothing

-- | Show your DMs, given how many to return and whether or not to use color.
showDMs count color filepath = showTweets color <$> getDMs count filepath

-- | Show a user profile given screen name, how many tweets to return (API
-- maximum is 3200), and whether to print them in color.
showProfile :: String -> Int -> Bool -> FilePath -> IO String
showProfile screenName count color filepath = showTweets color <$> getProfile screenName count filepath

-- | Show the most successful tweets by a given user, given their screen name. 
showBest :: String -> Bool -> FilePath -> IO String
showBest screenName color filepath = showTweets color . (fmap (take 13 . hits)) <$> getProfile screenName 3200 filepath

-- | Display user timeline
showTimeline :: Int -> Bool -> FilePath -> IO String
showTimeline count color filepath = showTweets color <$> getTimeline count filepath

-- | Display user timeline in color, as appropriate
showTweets :: Bool -> Either (ParseError Char Dec) Timeline -> String
showTweets color = (either show id) . (fmap (if color then displayTimelineColor else displayTimeline))

-- | Get user's DMs.
getDMs count filepath = do
    let requestString = "?count=" ++ (show count)
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest ("https://api.twitter.com/1.1/direct_messages.json" ++ requestString)
    request <- signRequest filepath $ initialRequest { method = "GET" }
    getTweets . BSL.unpack <$> responseBS request manager -- FIXME new type here?

-- | Get a timeline
getTimeline :: Int -> FilePath -> IO (Either (ParseError Char Dec) Timeline)
getTimeline count filepath = getTweets . BSL.unpack <$> getTimelineRaw count filepath

getTimelineRaw :: Int -> FilePath -> IO BSL.ByteString
getTimelineRaw count filepath = do
    let requestString = "?count=" ++ (show count)
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest ("https://api.twitter.com/1.1/statuses/home_timeline.json" ++ requestString)
    request <- signRequest filepath $ initialRequest { method = "GET" }
    responseBS request manager

deleteTweet = (fmap void) . deleteTweetRaw

-- | Delete a tweet given its id
deleteTweetRaw :: Int -> FilePath -> IO BSL.ByteString
deleteTweetRaw id filepath = do
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest ("https://api.twitter.com/1.1/statuses/destroy/" ++ (show id) ++ ".json")
    request <- signRequest filepath $ initialRequest { method = "POST" }
    responseBS request manager
