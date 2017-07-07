{-# LANGUAGE OverloadedStrings #-}

-- | Helper functions for the command line tool.
module Web.Tweet.API.Internal where

import Web.Tweet.API
import Web.Tweet.Types
import Web.Tweet.Utils
import Text.Megaparsec.Error

-- | Show a user profile given screen name, how many tweets to return, 
-- and whether to print them in color.
showProfile :: String -> Int -> Bool -> FilePath -> IO String
showProfile screenName count color = fmap (showTweets color) . getProfile screenName count

-- | Show the most successful tweets by a given user, given their screen name. 
showBest :: String -> Int -> Bool -> FilePath -> IO String
showBest screenName n color = fmap (showTweets color . pure . (take n . hits)) . getAll screenName Nothing

-- | Show the most successful tweets by a given user, given their screen name. Additionally filter out replies.
showBest' :: String -> Int -> Bool -> FilePath -> IO String
showBest' screenName n color = fmap (showTweets color . pure . (take n . hits')) . getAll screenName Nothing

-- | Display user timeline
showTimeline :: Int -> Bool -> FilePath -> IO String
showTimeline count color = (fmap (showTweets color)) . getTimeline count 

-- | Display user timeline in color, as appropriate
showTweets :: Bool -> Either (ParseError Char Dec) Timeline -> String
showTweets color = (either show id) . (fmap (if color then displayTimelineColor else displayTimeline))
