{-# LANGUAGE OverloadedStrings #-}

-- | Module containing the functions directly dealing with twitter's API
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
import Data.Composition

-- | Get tweets (text only) for some user
getMarkov :: String -> Maybe Int -> FilePath -> IO [String]
getMarkov = (fmap (map (view text))) .** getAll 

-- | Get all tweets by some user
getAll :: String -> Maybe Int -> FilePath -> IO Timeline
getAll screenName maxId filepath = do
    tweets <- either (error "Parse tweets failed") id <$> getProfileMax screenName 200 filepath maxId
    let lastId = _tweetId . last $ tweets
    if (Just lastId) == maxId then 
        pure []
    else
        do
            putStrLn $ "fetching tweets since " ++ show lastId ++ "..."
            next <- getAll screenName (Just lastId) filepath
            pure (tweets ++ next)

-- | tweet, given a `Tweet` and path to credentials. Return id of posted tweet.
tweetData :: Tweet -> FilePath -> IO Int
tweetData tweet filepath = do
    let requestString = urlString tweet
    bytes <- postRequest ("https://api.twitter.com/1.1/statuses/update.json" ++ requestString) filepath
    pure . (view tweetId) . head . either (error "failed to parse tweet") id . getTweets . BSL.unpack $ bytes

-- | Gets user profile with max_id set.
getProfileMax :: String -> Int -> FilePath -> Maybe Int -> IO (Either (ParseError Char Dec) Timeline)
getProfileMax screenName count filepath maxId = getTweets . BSL.unpack <$> getProfileRaw screenName count filepath maxId

-- | Gets user profile with max_id set.
getProfileRaw :: String -> Int -> FilePath -> Maybe Int -> IO BSL.ByteString
getProfileRaw screenName count filepath maxId = getRequest ("https://api.twitter.com/1.1/statuses/user_timeline.json" ++ requestString) filepath
    where requestString = case maxId of {
        (Just id) -> "?screen_name=" ++ screenName ++ "&count=" ++ (show count) ++ "&max_id=" ++ (show id) ;
        Nothing -> "?screen_name=" ++ screenName ++ "&count=" ++ (show count) }

-- | Get user profile given screen name and how many tweets to return
getProfile :: String -> Int -> FilePath -> IO (Either (ParseError Char Dec) Timeline)
getProfile screenName count filepath = getProfileMax screenName count filepath Nothing

-- | Show a user profile given screen name, how many tweets to return, 
-- and whether to print them in color.
showProfile :: String -> Int -> Bool -> FilePath -> IO String
showProfile screenName count color filepath = showTweets color <$> getProfile screenName count filepath

-- | Show the most successful tweets by a given user, given their screen name. 
showBest :: String -> Bool -> FilePath -> IO String
showBest screenName color filepath = showTweets color . pure . (take 13 . hits) <$> getAll screenName Nothing filepath 
-- (fmap (take 13 . hits)) <$> getProfile screenName 3200 filepath

-- | Display user timeline
showTimeline :: Int -> Bool -> FilePath -> IO String
showTimeline count color = (fmap (showTweets color)) . getTimeline count 

-- | Display user timeline in color, as appropriate
showTweets :: Bool -> Either (ParseError Char Dec) Timeline -> String
showTweets color = (either show id) . (fmap (if color then displayTimelineColor else displayTimeline))

-- | Get user's DMs.
getDMsRaw count = getRequest ("https://api.twitter.com/1.1/direct_messages.json" ++ requestString)
    where requestString = "?count=" ++ (show count)

-- | Get a timeline
getTimeline :: Int -> FilePath -> IO (Either (ParseError Char Dec) Timeline)
getTimeline = (fmap (getTweets . BSL.unpack)) .* getTimelineRaw

-- | Get a user's timeline and return response as a bytestring
getTimelineRaw :: Int -> FilePath -> IO BSL.ByteString
getTimelineRaw count = getRequest ("https://api.twitter.com/1.1/statuses/home_timeline.json" ++ requestString)
    where requestString = "?count=" ++ (show count)

-- | Delete a tweet given its id
deleteTweet :: Integer -> FilePath -> IO ()
deleteTweet = (fmap void) . deleteTweetRaw

-- | Favorite a tweet given its id
favoriteTweet :: Integer -> FilePath -> IO ()
favoriteTweet = (fmap void) . favoriteTweetRaw

-- | Unfavorite a tweet given its id
unfavoriteTweet :: Integer -> FilePath -> IO ()
unfavoriteTweet = (fmap void) . unfavoriteTweetRaw

-- | Unretweet a tweet given its id
unretweetTweet :: Integer -> FilePath -> IO ()
unretweetTweet = (fmap void) . unretweetTweetRaw

-- | Unfollow a user given their screen name
unfollow :: String -> FilePath -> IO ()
unfollow = (fmap void) . unfollowUserRaw

-- | Follow a user given their screen name
follow :: String -> FilePath -> IO ()
follow = (fmap void) . followUserRaw

-- | Retweet a tweet given its id
retweetTweet :: Integer -> FilePath -> IO ()
retweetTweet = (fmap void) . retweetTweetRaw

-- | Favorite a tweet given its id; return bytestring response
favoriteTweetRaw :: Integer -> FilePath -> IO BSL.ByteString
favoriteTweetRaw id = postRequest ("https://api.twitter.com/1.1/favorites/destroy/" ++ (show id) ++ ".json")

-- | Retweet a tweet given its id; return bytestring response
retweetTweetRaw :: Integer -> FilePath -> IO BSL.ByteString
retweetTweetRaw id = postRequest ("https://api.twitter.com/1.1/statuses/retweet/" ++ (show id) ++ ".json")

-- | Send a DM given text, screen name of recipient.
sendDMRaw txt screenName = postRequest ("https://api.twitter.com/1.1/direct_messages/new.json?text=" ++ encoded ++ "&screen_name" ++ screenName ++ ".json")
    where encoded = strEncode $ txt

-- | Follow a user given their screen name
followUserRaw :: String -> FilePath -> IO BSL.ByteString
followUserRaw screenName = postRequest("https://api.twitter.com/1.1/friendships/create.json?screen_name=" ++ screenName)

-- | Follow a user given their screen name
unfollowUserRaw :: String -> FilePath -> IO BSL.ByteString
unfollowUserRaw screenName = postRequest("https://api.twitter.com/1.1/friendships/destroy.json?screen_name=" ++ screenName)

-- | Unretweet a tweet given its id; return bytestring response
unretweetTweetRaw :: Integer -> FilePath -> IO BSL.ByteString
unretweetTweetRaw id = postRequest ("https://api.twitter.com/1.1/statuses/unretweet/" ++ (show id) ++ ".json")

-- | Favorite a tweet given its id; return bytestring response
unfavoriteTweetRaw :: Integer -> FilePath -> IO BSL.ByteString
unfavoriteTweetRaw id = postRequest ("https://api.twitter.com/1.1/favorites/destroy/" ++ (show id) ++ ".json")

-- | Delete a tweet given its id; return bytestring response
deleteTweetRaw :: Integer -> FilePath -> IO BSL.ByteString
deleteTweetRaw id = postRequest ("https://api.twitter.com/1.1/statuses/destroy/" ++ (show id) ++ ".json")
