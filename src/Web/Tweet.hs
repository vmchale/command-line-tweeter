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
    , getTimeline
    , showTimeline
    , getProfile
    , showProfile
    , showBest
    , getDMs
    , showDMs
    , getRaw
    ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Char
import Web.Tweet.Types
import Web.Tweet.Utils
import Control.Monad
import Data.List.Split (chunksOf)
import Data.Maybe
import Control.Lens
import Control.Lens.Tuple
import Web.Authenticate.OAuth
import Web.Tweet.Sign
import Data.List.Utils
import Text.Megaparsec.Error

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
            y@(x:xs) -> thread' y hs (Just 0) num filepath

-- | Helper function to make `thread` easier to write. 
thread' :: [String] -> [String] -> Maybe Int -> Int -> FilePath -> IO ()
thread' content hs idNum num filepath = do
    -- fix the stuff with the handles.
    let f = \str i -> tweetData (Tweet { _status = str, _trimUser = True, _handles = hs, _replyID = if i == 0 then Nothing else Just i }) filepath
    let initial = f (head content)
    last <- foldr ((>=>) . f) initial (content) $ fromMaybe 0 idNum
    deleteTweet last filepath

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
mkTweet contents = over (status) (const (contents)) def 

-- | tweet, given a `Tweet` and path to credentials. Return id of posted tweet.
tweetData :: Tweet -> FilePath -> IO Int
tweetData tweet filepath = do
    let requestString = urlString tweet
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest ("https://api.twitter.com/1.1/statuses/update.json" ++ requestString)
    request <- signRequest filepath $ initialRequest { method = "POST" }
    responseInt request manager

-- | Get tweets (text only) for some user
-- TODO make it recursive/have it read max_id from returned tweet.
getRaw :: String -> Maybe Int -> FilePath -> IO [String]
getRaw screenName maxId filepath = do
    tweets <- either (error "Parse tweets failed") id <$> getProfileMax screenName 200 filepath maxId
    let lastId = _tweetId . last $ tweets
    if (Just lastId) == maxId then 
        pure []
    else
        do
            putStrLn $ "fetching tweets since " ++ show lastId ++ "..."
            next <- getRaw screenName (Just lastId) filepath
            pure ((map _text tweets) ++ next)

getProfileMax :: String -> Int -> FilePath -> Maybe Int -> IO (Either (ParseError Char Dec) Timeline)
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

-- | Display user timeline in color
showTweets :: Bool -> Either (ParseError Char Dec) Timeline -> String
showTweets color = (either show id) . (fmap (if color then displayTimelineColor else displayTimeline))

-- | Get user's DMs.
getDMs count filepath = do
    let requestString = "?count=" ++ (show count)
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest ("https://api.twitter.com/1.1/direct_messages.json" ++ requestString)
    request <- signRequest filepath $ initialRequest { method = "GET" }
    getTweets . BSL.unpack <$> responseBS request manager

-- | Get a timeline
getTimeline :: Int -> FilePath -> IO (Either (ParseError Char Dec) Timeline)
getTimeline count filepath = do
    let requestString = "?count=" ++ (show count)
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest ("https://api.twitter.com/1.1/statuses/home_timeline.json" ++ requestString)
    request <- signRequest filepath $ initialRequest { method = "GET" }
    getTweets . BSL.unpack <$> responseBS request manager

-- | Delete a tweet given its id
deleteTweet id filepath = do
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest ("https://api.twitter.com/1.1/statuses/destroy/" ++ (show id) ++ ".json")
    request <- signRequest filepath $ initialRequest { method = "POST" }
    void $ responseBS request manager

-- | Return HTTP request's result as a bytestring
responseBS :: Request -> Manager -> IO BSL.ByteString
responseBS request manager = do
    response <- httpLbs request manager
    let code = statusCode $ responseStatus response
    putStr $ if (code == 200) then "" else "failed :(\n error code: " ++ (show code) ++ "\n"
    pure . responseBody $ response

-- | print output of a request and return status id as an `Int`. 
responseInt :: Request -> Manager -> IO Int
responseInt request manager = do
    response <- httpLbs request manager
    let code = statusCode $ responseStatus response
    putStrLn $ if (code == 200) then "POST succesful!" else "failed :(\n error code: " ++ (show code)
    return $ (read . (takeWhile (/=',')) . (drop 52)) (BSL.unpack $ responseBody response)

-- | Convert a tweet to a percent-encoded url for querying an API
-- TODO make this more general, taking in a Map object
urlString :: Tweet -> String
urlString tweet = concat [ "?status="
                         , BS.unpack (tweetEncode tweet)
                         , "&trim_user="
                         , map toLower (show trim)
                         , (if isJust (_replyID tweet) then "&in_reply_to_status_id=" else "")
                         , reply ]
    where trim  = _trimUser tweet
          reply = fromMaybe "" (show <$> _replyID tweet)

-- | Percent-encode the status update so it's fit for a URL and UTF-encode it as well. 
tweetEncode :: Tweet -> BS.ByteString
tweetEncode tweet = paramEncode . encodeUtf8 $ handleStr `T.append` content
    where content   = T.pack . _status $ tweet
          handleStr = T.pack $ concatMap ((++ " ") . ((++) "@")) hs
          hs        = _handles tweet
