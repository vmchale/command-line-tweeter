-- | Helper functions for the command line tool.
module Web.Tweet.API.Internal where

import           Data.Void
import           Text.Megaparsec.Error
import           Web.Tweet.API
import           Web.Tweet.Types
import           Web.Tweet.Utils

type Filter = Timeline -> Timeline

-- | Show a user profile given screen name, how many tweets to return,
-- and whether to print them in color.
showProfile :: String -> Int -> Bool -> FilePath -> IO String
showProfile sn count color = fmap (showTweets color) . getProfile sn count

-- | Show the most successful tweets by a given user, given their screen name.
showBest :: String -> Int -> Bool -> FilePath -> IO String
showBest sn n color = fmap (showTweets color . pure . take n . hits) . getAll sn Nothing

-- | Show the most successful tweets by a given user, given their screen name. Additionally filter out replies.
showBest' :: String -> Int -> Bool -> FilePath -> IO String
showBest' sn n color = fmap (showTweets color . pure . take n . hits') . getAll sn Nothing

-- | Display user timeline
showTimeline :: Int -> Bool -> FilePath -> IO String
showTimeline count color = fmap (showTweets color) . getTimeline count

showFilteredTL :: [Filter] -> String -> Int -> Bool -> FilePath -> IO String
showFilteredTL filters sn count color = fmap (showTweets color . fmap (foldr (.) id filters)) . getProfile sn count

-- | Display user timeline in color, as appropriate
showTweets :: Bool -> Either (ParseError Char Void) Timeline -> String
showTweets color = either show id . fmap (if color then displayTimelineColor else displayTimeline)

-- | Display a user's favorites
showFavorites :: Int -> String -> Bool -> FilePath -> IO String
showFavorites count sn color = fmap (showTweets color) . getFavorites count sn
