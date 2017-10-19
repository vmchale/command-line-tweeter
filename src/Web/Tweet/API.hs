{-# LANGUAGE OverloadedStrings #-}

-- | Module containing the functions directly dealing with twitter's API. Most functions in this module have two versions - one which takes a path to a TOML file containing api keys/secrets and tokens/secrets, the other takes api keys/secrets and tokens/secrets as an argument.
module Web.Tweet.API where

import           Control.Composition
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Void
import           Text.Megaparsec.Error
import           Web.Tweet.Types
import           Web.Tweet.Utils
import           Web.Tweet.Utils.API

-- | Get tweets (text only) for some user
getMarkov :: String -> Maybe Int -> FilePath -> IO [String]
getMarkov = (fmap (map (view text))) .** getAll

-- | Get all tweets by some user
getAll :: String -> Maybe Int -> FilePath -> IO Timeline
getAll sn maxId filepath = do
    tweets <- either (error "Parse tweets failed") id <$> getProfileMax sn 200 filepath maxId
    let lastId = _tweetId . last $ tweets
    if (Just lastId) == maxId then
        pure []
    else
        do
            putStrLn $ "fetching tweets since " ++ show lastId ++ "..."
            next <- getAll sn (Just lastId) filepath
            pure (tweets ++ next)

-- | tweet, given a `Tweet` and a `Config` containing necessary data to sign the request.
tweetDataMem :: Tweet -> Config -> IO Int
tweetDataMem tweet config = do
    let requestString = urlString tweet
    bytes <- postRequestMem ("https://api.twitter.com/1.1/statuses/update.json" ++ requestString) config
    putStrLn $ displayTimelineColor . either (error "failed to parse tweet") id . getTweets . BSL.toStrict $ bytes
    pure . (view tweetId) . head . either (error "failed to parse tweet") id . getTweets . BSL.toStrict $ bytes

-- | tweet, given a `Tweet` and path to credentials. Return id of posted tweet.
tweetData :: Tweet -> FilePath -> IO Int
tweetData tweet filepath = do
    let requestString = urlString tweet
    bytes <- postRequest ("https://api.twitter.com/1.1/statuses/update.json" ++ requestString) filepath -- FIXME fix the coloration
    putStrLn $ displayTimelineColor . either (error "failed to parse tweet") id . getTweets . BSL.toStrict $ bytes
    pure . (view tweetId) . head . either (error "failed to parse tweet") id . getTweets . BSL.toStrict $ bytes

-- | Gets user profile with max_id set.
getProfileMax :: String -> Int -> FilePath -> Maybe Int -> IO (Either (ParseError Char Void) Timeline)
getProfileMax = fmap (getTweets . BSL.toStrict) .*** getProfileRaw

-- | Gets user profile with max_id set.
getProfileMaxMem :: String -> Int -> Config -> Maybe Int -> IO (Either (ParseError Char Void) Timeline)
getProfileMaxMem = fmap (getTweets . BSL.toStrict) .*** getProfileRawMem

-- | Gets user profile with max_id set.
getProfileRaw :: String -> Int -> FilePath -> Maybe Int -> IO BSL.ByteString
getProfileRaw sn count filepath maxId = getRequest ("https://api.twitter.com/1.1/statuses/user_timeline.json" ++ requestString) filepath
    where requestString = case maxId of {
        (Just i) -> "?screen_name=" ++ sn ++ "&count=" ++ (show count) ++ "&max_id=" ++ (show i) ;
        Nothing -> "?screen_name=" ++ sn ++ "&count=" ++ (show count) }

-- | Gets user profile with max_id set
getProfileRawMem :: String -> Int -> Config -> Maybe Int -> IO BSL.ByteString
getProfileRawMem sn count config maxId = getRequestMem ("https://api.twitter.com/1.1/statuses/user_timeline.json" ++ requestString) config
    where requestString = case maxId of {
        (Just i) -> "?screen_name=" ++ sn ++ "&count=" ++ (show count) ++ "&max_id=" ++ (show i) ;
        Nothing -> "?screen_name=" ++ sn ++ "&count=" ++ (show count) }

-- | Get mentions and parse response as a list of tweets
mentions :: Int -> FilePath -> IO (Either (ParseError Char Void) Timeline)
mentions = fmap (getTweets . BSL.toStrict) .* mentionsRaw

-- | Get mentions and parse response as a list of tweets
mentionsMem :: Int -> Config -> IO (Either (ParseError Char Void) Timeline)
mentionsMem = fmap (getTweets . BSL.toStrict) .* mentionsRawMem

-- | Gets mentions
mentionsRaw :: Int -> FilePath -> IO BSL.ByteString
mentionsRaw count = getRequest ("https://api.twitter.com/1.1/statuses/mentions_timeline.json" ++ requestString)
    where requestString = "?count=" ++ (show count)

-- | Gets mentions
mentionsRawMem :: Int -> Config -> IO BSL.ByteString
mentionsRawMem count = getRequestMem ("https://api.twitter.com/1.1/statuses/mentions_timeline.json" ++ requestString)
    where requestString = "?count=" ++ (show count)

-- | Get user profile given screen name and how many tweets to return
getProfile :: String -> Int -> FilePath -> IO (Either (ParseError Char Void) Timeline)
getProfile sn count filepath = getProfileMax sn count filepath Nothing

-- | Mute a user given their screen name
mute :: String -> FilePath -> IO ()
mute = (fmap void) . muteUserRaw

-- | Mute a user given their screen name
muteMem :: String -> Config -> IO ()
muteMem = (fmap void) . muteUserRawMem

-- | Unmute a user given their screen name
unmute :: String -> FilePath -> IO ()
unmute = (fmap void) . unmuteUserRaw

-- | Unmute a user given their screen name
unmuteMem :: String -> Config -> IO ()
unmuteMem = (fmap void) . unmuteUserRawMem

-- | Mute a user given their screen name
muteUserRaw :: String -> FilePath -> IO BSL.ByteString
muteUserRaw sn = postRequest ("https://api.twitter.com/1.1/mutes/users/create.json?screen_name=" ++ sn)

-- | Mute a user given their screen name
muteUserRawMem :: String -> Config -> IO BSL.ByteString
muteUserRawMem sn = postRequestMem ("https://api.twitter.com/1.1/mutes/users/create.json?screen_name=" ++ sn)

-- | Unmute a user given their screen name
unmuteUserRaw :: String -> FilePath -> IO BSL.ByteString
unmuteUserRaw sn = postRequest ("https://api.twitter.com/1.1/mutes/users/destroy.json?screen_name=" ++ sn)

-- | Unmute a user given their screen name
unmuteUserRawMem :: String -> Config -> IO BSL.ByteString
unmuteUserRawMem sn = postRequestMem ("https://api.twitter.com/1.1/mutes/users/destroy.json?screen_name=" ++ sn)

-- | Get user's DMs.
getDMsRaw :: Show p => p -> FilePath -> IO BSL.ByteString
getDMsRaw count = getRequest ("https://api.twitter.com/1.1/direct_messages.json" ++ requestString)
    where requestString = "?count=" ++ (show count)

-- | Get a user's favorites
getFavorites :: Int -> String -> FilePath -> IO (Either (ParseError Char Void) Timeline)
getFavorites count = fmap (fmap (take count)) . (fmap (getTweets . BSL.toStrict)) .* favoriteTweetListRaw

-- | Get a timeline
getTimeline :: Int -> FilePath -> IO (Either (ParseError Char Void) Timeline)
getTimeline = (fmap (getTweets . BSL.toStrict)) .* getTimelineRaw

-- | Get a timeline
getTimelineMem :: Int -> Config -> IO (Either (ParseError Char Void) Timeline)
getTimelineMem = (fmap (getTweets . BSL.toStrict)) .* getTimelineRawMem

-- | Get a user's timeline and return response as a bytestring
getTimelineRaw :: Int -> FilePath -> IO BSL.ByteString
getTimelineRaw count = getRequest ("https://api.twitter.com/1.1/statuses/home_timeline.json" ++ requestString)
    where requestString = "?count=" ++ (show count)

-- | Get a user's timeline and return response as a bytestring
getTimelineRawMem :: Int -> Config -> IO BSL.ByteString
getTimelineRawMem count = getRequestMem ("https://api.twitter.com/1.1/statuses/home_timeline.json" ++ requestString)
    where requestString = "?count=" ++ (show count)

-- | Delete a tweet given its id
deleteTweet :: Integer -> FilePath -> IO ()
deleteTweet = (fmap void) . deleteTweetRaw

-- | Delete a tweet given its id
deleteTweetMem :: Integer -> Config -> IO ()
deleteTweetMem = (fmap void) . deleteTweetRawMem

-- | Get response, i.e. the tweet deleted
deleteTweetResponse :: Integer -> FilePath -> IO (Either (ParseError Char Void) Timeline)
deleteTweetResponse = fmap (getTweets . BSL.toStrict) .* deleteTweetRaw

-- | Get response, i.e. the tweet deleted
deleteTweetResponseMem :: Integer -> Config -> IO (Either (ParseError Char Void) Timeline)
deleteTweetResponseMem = fmap (getTweets . BSL.toStrict) .* deleteTweetRawMem

-- | Favorite a tweet given its id
favoriteTweet :: Integer -> FilePath -> IO ()
favoriteTweet = (fmap void) . favoriteTweetRaw

-- | Favorite a tweet given its id
favoriteTweetMem :: Integer -> Config -> IO ()
favoriteTweetMem = (fmap void) . favoriteTweetRawMem

-- | Favorite a tweet and returned the (parsed) response
favoriteTweetList :: String -> FilePath -> IO (Either (ParseError Char Void) Timeline)
favoriteTweetList = fmap (getTweets . BSL.toStrict) .* favoriteTweetListRaw

-- | Favorite a tweet and returned the (parsed) response
favoriteTweetListMem :: String -> Config -> IO (Either (ParseError Char Void) Timeline)
favoriteTweetListMem = fmap (getTweets . BSL.toStrict) .* favoriteTweetListRawMem

-- | Favorite a tweet and returned the (parsed) response
favoriteTweetResponse :: Integer -> FilePath -> IO (Either (ParseError Char Void) Timeline)
favoriteTweetResponse = fmap (getTweets . BSL.toStrict) .* favoriteTweetRaw

-- | Unfavorite a tweet given its id
unfavoriteTweet :: Integer -> FilePath -> IO ()
unfavoriteTweet = (fmap void) . unfavoriteTweetRaw

-- | Unfavorite a tweet given its id
unfavoriteTweetMem :: Integer -> Config -> IO ()
unfavoriteTweetMem = (fmap void) . unfavoriteTweetRawMem

-- | Unfavorite a tweet and returned the (parsed) response
unfavoriteTweetResponse :: Integer -> FilePath -> IO (Either (ParseError Char Void) Timeline)
unfavoriteTweetResponse = fmap (getTweets . BSL.toStrict) .* unfavoriteTweetRaw

-- | Unfavorite a tweet and returned the (parsed) response
unfavoriteTweetResponseMem :: Integer -> Config -> IO (Either (ParseError Char Void) Timeline)
unfavoriteTweetResponseMem = fmap (getTweets . BSL.toStrict) .* unfavoriteTweetRawMem

-- | Unretweet a tweet given its id
unretweetTweet :: Integer -> FilePath -> IO ()
unretweetTweet = (fmap void) . unretweetTweetRaw

-- | Unretweet a tweet given its id
unretweetTweetMem :: Integer -> Config -> IO ()
unretweetTweetMem = (fmap void) . unretweetTweetRawMem

-- | Unretweet a tweet and returned the (parsed) response
unretweetTweetResponse :: Integer -> FilePath -> IO (Either (ParseError Char Void) Timeline)
unretweetTweetResponse = fmap (getTweets . BSL.toStrict) .* unretweetTweetRaw

-- | Unretweet a tweet and returned the (parsed) response
unretweetTweetResponseMem :: Integer -> Config -> IO (Either (ParseError Char Void) Timeline)
unretweetTweetResponseMem = fmap (getTweets . BSL.toStrict) .* unretweetTweetRawMem

-- | Unfollow a user given their screen name
unfollow :: String -> FilePath -> IO ()
unfollow = (fmap void) . unfollowUserRaw

-- | Unfollow a user given their screen name
unfollowMem :: String -> Config -> IO ()
unfollowMem = (fmap void) . unfollowUserRawMem

-- | Follow a user given their screen name
follow :: String -> FilePath -> IO ()
follow = (fmap void) . followUserRaw

-- | Follow a user given their screen name
followMem :: String -> Config -> IO ()
followMem = (fmap void) . followUserRawMem

-- | Block a user given their screen name
block :: String -> FilePath -> IO ()
block = (fmap void) . blockUserRaw

-- | Block a user given their screen name
blockMem :: String -> Config -> IO ()
blockMem = (fmap void) . blockUserRawMem

-- | Unblock a user given their screen name
unblock :: String -> FilePath -> IO ()
unblock = (fmap void) . unblockUserRaw

-- | Unblock a user given their screen name
unblockMem :: String -> Config -> IO ()
unblockMem = (fmap void) . unblockUserRawMem

-- | Retweet a tweet given its id
retweetTweet :: Integer -> FilePath -> IO ()
retweetTweet = (fmap void) . retweetTweetRaw

-- | Retweet a tweet given its id
retweetTweetMem :: Integer -> Config -> IO ()
retweetTweetMem = (fmap void) . retweetTweetRawMem

-- | Retweet a tweet and returned the (parsed) response
retweetTweetResponse :: Integer -> FilePath -> IO (Either (ParseError Char Void) Timeline)
retweetTweetResponse = fmap (getTweets . BSL.toStrict) .* retweetTweetRaw

-- | Retweet a tweet and returned the (parsed) response
retweetTweetResponseMem :: Integer -> Config -> IO (Either (ParseError Char Void) Timeline)
retweetTweetResponseMem = fmap (getTweets . BSL.toStrict) .* retweetTweetRawMem

-- | Get a lisr of favorited tweets by screen name; return bytestring response
favoriteTweetListRaw :: String -> FilePath -> IO BSL.ByteString
favoriteTweetListRaw sn = getRequest ("https://api.twitter.com/1.1/favorites/list.json?screen_name=" ++ sn)

-- | Get a lisr of favorited tweets by screen name; return bytestring response
favoriteTweetListRawMem :: String -> Config -> IO BSL.ByteString
favoriteTweetListRawMem sn = getRequestMem ("https://api.twitter.com/1.1/favorites/list.json?screen_name=" ++ sn)

-- | Favorite a tweet given its id; return bytestring response
favoriteTweetRaw :: Integer -> FilePath -> IO BSL.ByteString
favoriteTweetRaw idNum = postRequest ("https://api.twitter.com/1.1/favorites/create.json?id=" ++ (show idNum))

-- | Favorite a tweet given its idNum; return bytestring response
favoriteTweetRawMem :: Integer -> Config -> IO BSL.ByteString
favoriteTweetRawMem idNum = postRequestMem ("https://api.twitter.com/1.1/favorites/create.json?id=" ++ (show idNum))

-- | Retweet a tweet given its idNum; return bytestring response
retweetTweetRaw :: Integer -> FilePath -> IO BSL.ByteString
retweetTweetRaw idNum = postRequest ("https://api.twitter.com/1.1/statuses/retweet/" ++ (show idNum) ++ ".json")

-- | Retweet a tweet given its idNum; return bytestring response
retweetTweetRawMem :: Integer -> Config -> IO BSL.ByteString
retweetTweetRawMem idNum = postRequestMem ("https://api.twitter.com/1.1/statuses/retweet/" ++ (show idNum) ++ ".json")

-- | Send a DM given text, screen name of recipient.
sendDMRaw :: String -> [Char] -> FilePath -> IO BSL.ByteString
sendDMRaw txt sn = postRequest ("https://api.twitter.com/1.1/direct_messages/new.json?text=" ++ encoded ++ "&screen_name" ++ sn ++ ".json")
    where encoded = strEncode txt

-- | Get DMs, return bytestring of response
getDMs :: Int -> FilePath -> IO BSL.ByteString
getDMs count = getRequest ("https://dev.twitter.com/rest/reference/get/direct_messages.json?count=" ++ (show count))

-- | Get DMs, return bytestring of response
getDMMem :: Int -> Config -> IO BSL.ByteString
getDMMem count = getRequestMem ("https://dev.twitter.com/rest/reference/get/direct_messages.json?count=" ++ (show count))

-- | Follow a user given their screen name
followUserRaw :: String -> FilePath -> IO BSL.ByteString
followUserRaw sn = postRequest ("https://api.twitter.com/1.1/friendships/create.json?screen_name=" ++ sn)

-- | Follow a user given their screen name
followUserRawMem :: String -> Config -> IO BSL.ByteString
followUserRawMem sn = postRequestMem ("https://api.twitter.com/1.1/friendships/create.json?screen_name=" ++ sn)

-- | Block a user given their screen name
blockUserRaw :: String -> FilePath -> IO BSL.ByteString
blockUserRaw sn = postRequest ("https://api.twitter.com/1.1/blocks/create.json?screen_name=" ++ sn)

-- | Block a user given their screen name
blockUserRawMem :: String -> Config -> IO BSL.ByteString
blockUserRawMem sn = postRequestMem ("https://api.twitter.com/1.1/blocks/create.json?screen_name=" ++ sn)

-- | Unblock a user given their screen name
unblockUserRaw :: String -> FilePath -> IO BSL.ByteString
unblockUserRaw sn = postRequest ("https://api.twitter.com/1.1/blocks/destroy.json?screen_name=" ++ sn)

-- | Unblock a user given their screen name
unblockUserRawMem :: String -> Config -> IO BSL.ByteString
unblockUserRawMem sn = postRequestMem ("https://api.twitter.com/1.1/blocks/destroy.json?screen_name=" ++ sn)

-- | Follow a user given their screen name
unfollowUserRaw :: String -> FilePath -> IO BSL.ByteString
unfollowUserRaw sn = postRequest ("https://api.twitter.com/1.1/friendships/destroy.json?screen_name=" ++ sn)

-- | Follow a user given their screen name
unfollowUserRawMem :: String -> Config -> IO BSL.ByteString
unfollowUserRawMem sn = postRequestMem ("https://api.twitter.com/1.1/friendships/destroy.json?screen_name=" ++ sn)

-- | Unretweet a tweet given its id; return bytestring response
unretweetTweetRaw :: Integer -> FilePath -> IO BSL.ByteString
unretweetTweetRaw idNum = postRequest ("https://api.twitter.com/1.1/statuses/unretweet/" ++ (show idNum) ++ ".json")

-- | Unretweet a tweet given its idNum; return bytestring response
unretweetTweetRawMem :: Integer -> Config -> IO BSL.ByteString
unretweetTweetRawMem idNum = postRequestMem ("https://api.twitter.com/1.1/statuses/unretweet/" ++ (show idNum) ++ ".json")

-- | Unfavorite a tweet given its idNum; return bytestring response
unfavoriteTweetRaw :: Integer -> FilePath -> IO BSL.ByteString
unfavoriteTweetRaw idNum = postRequest ("https://api.twitter.com/1.1/favorites/destroy.json?id=" ++ (show idNum))

-- | Unfavorite a tweet given its idNum; return bytestring response
unfavoriteTweetRawMem :: Integer -> Config -> IO BSL.ByteString
unfavoriteTweetRawMem idNum = postRequestMem ("https://api.twitter.com/1.1/favorites/destroy.json?id=" ++ (show idNum))

-- | Delete a tweet given its idNum; return bytestring response
deleteTweetRaw :: Integer -> FilePath -> IO BSL.ByteString
deleteTweetRaw idNum = postRequest ("https://api.twitter.com/1.1/statuses/destroy/" ++ (show idNum) ++ ".json")

-- | Delete a tweet given its idNum; return bytestring response
deleteTweetRawMem :: Integer -> Config -> IO BSL.ByteString
deleteTweetRawMem idNum = postRequestMem ("https://api.twitter.com/1.1/statuses/destroy/" ++ (show idNum) ++ ".json")
