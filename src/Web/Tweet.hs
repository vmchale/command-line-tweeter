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
    , thread
    -- * Data type for a tweet
    , module Web.Tweet.Types
    -- * Various API calls
    , module Web.Tweet.API
    -- * Functions to sign API requests
    , signRequest
    -- * Functions to generate a URL string from a `Tweet`
    , urlString
    ) where
    
import Web.Tweet.Sign
import Web.Tweet.API
import Web.Tweet.Utils.API
import Web.Tweet.Types
import Web.Tweet.Utils
import Data.List.Split (chunksOf)
import Control.Monad
import Control.Lens
import Data.Maybe
import Data.Default

-- | Tweet a string given a path to credentials; return the id of the status.
--
-- > basicTweet "On the airplane." ".cred"
basicTweet :: String -> FilePath -> IO Int
basicTweet contents = tweetData (mkTweet contents)

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
    let f = \str i -> tweetData (Tweet { _status = str, _handles = hs, _replyID = if i == 0 then Nothing else Just i }) filepath
    let initial = f (head content)
    last <- foldr ((>=>) . f) initial (content) $ fromMaybe 0 idNum
    deleteTweet (fromIntegral last) filepath

-- | Reply with a single tweet. Works the same as `thread` but doesn't take the fourth argument.
--
-- > reply "Idk what that means" ["friend1"] (Just 189943500) ".cred"
reply :: String -> [String] -> Maybe Int -> FilePath -> IO ()
reply contents hs idNum = thread contents hs idNum 1

-- | Make a `Tweet` with only the contents.
mkTweet :: String -> Tweet
mkTweet contents = over (status) (const (contents)) def 
