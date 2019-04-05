-- | Various utilities to tweet using the twitter api
--
-- Make sure you have a file credentials file (default the executable looks for is @$HOME/.cred.toml@) with the following info:
--
-- @
--
-- api-key = "API_KEY"
--
-- api-sec = "API_SECRET"
--
-- tok = "OAUTH_TOKEN"
--
-- tok-sec = "TOKEN_SECRET"
--
-- @

module Web.Tweet
    (
    -- * Functions to tweet
      basicTweet
    , thread
    , reply
    -- * Data type for a tweet
    , module Web.Tweet.Types
    -- * Various API calls
    , module Web.Tweet.API
    , module Web.Tweet.API.Internal
    -- * Functions to sign API requests
    , signRequest
    , oAuthMem
    , credentialMem
    -- * Functions to generate a URL string from a `Tweet`
    , urlString
    -- * Timeline filters
    , filterReplies
    , filterRTs
    , filterQuotes
    -- * Helper function to print a bird
    , bird
    ) where

import           Control.Monad
import           Data.List.Split        (chunksOf)
import           Data.Maybe
import           Lens.Micro
import           Web.Tweet.API
import           Web.Tweet.API.Internal
import           Web.Tweet.Sign
import           Web.Tweet.Types
import           Web.Tweet.Utils
import           Web.Tweet.Utils.API

-- | Tweet a string given a path to credentials; return the id of the status.
--
-- > basicTweet "On the airplane." ".cred.toml"
basicTweet :: String -> FilePath -> IO Int
basicTweet contents = tweetData (mkTweet contents)

-- | thread tweets together nicely. Takes a string, a list of handles to reply to, plus the ID of the status you're replying to.
-- If you need to thread tweets without replying, pass a `Nothing` as the third argument.
--
-- > thread "Hi I'm back in New York!" ["friend1","friend2"] Nothing 1 ".cred"
thread :: String -> [String] -> Maybe Int -> Int -> FilePath -> IO ()
thread contents hs idNum num filepath = do
    let handleStr = concatMap ((++) " " . (++) "@") hs
    let content = take num . chunksOf (280-length handleStr) $ contents
    case idNum of
        (Just _) -> thread' content hs idNum filepath
        Nothing -> case content of
            []      -> pure ()
            [x]     -> void $ basicTweet x filepath
            y@(_:_) -> thread' y hs (Just 0) filepath

-- | Helper function to make `thread` easier to write.
thread' :: [String] -> [String] -> Maybe Int -> FilePath -> IO ()
thread' content hs idNum filepath = do
    let f str i = tweetData Tweet { _status = str, _handles = hs, _replyID = if i == 0 then Nothing else Just i } filepath
    let initial = f (head content)
    lastTweet <- foldr ((>=>) . f) initial content $ fromMaybe 0 idNum
    deleteTweet (fromIntegral lastTweet) filepath

-- | Reply with a single tweet. Works the same as `thread` but doesn't take the fourth argument.
--
-- > reply "Idk what that means" ["friend1"] (Just 189943500) ".cred"
reply :: String -> [String] -> Maybe Int -> FilePath -> IO ()
reply contents hs idNum = thread contents hs idNum 1

-- | Make a `Tweet` with only the contents.
mkTweet :: String -> Tweet
mkTweet contents = over status (pure contents) pricklyTweet
