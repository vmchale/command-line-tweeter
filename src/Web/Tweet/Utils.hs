-- | Miscellaneous functions that don't fit the project directly
module Web.Tweet.Utils (
    hits
  , hits'
  , getTweets
  , displayTimeline
  , displayTimelineColor
  , lineByKey
  , bird
  , getConfigData
  , filterQuotes
  , filterReplies
  , filterRTs
  ) where

import           Control.Composition
import qualified Data.ByteString        as BS2
import qualified Data.ByteString.Char8  as BS
import           Data.List
import           Data.List.Extra
import           Data.Void
import           Lens.Micro.Extras
import           Text.Megaparsec
import           Web.Tweet.Parser
import           Web.Tweet.Types
import           Web.Tweet.Utils.Colors

-- | filter out retweets, and sort by most successful.
hits :: Timeline -> Timeline
hits = sortTweets . filterRTs

-- | Filter out retweets and replies, and sort by most sucessful.
hits' :: Timeline -> Timeline
hits' = hits . filterReplies

-- | Filter out retweets
filterRTs :: Timeline -> Timeline
filterRTs = filter ((/="RT @") . take 4 . view text)

-- | Filter out replies
filterReplies :: Timeline -> Timeline
filterReplies = filter ((/="@") . take 1 . view text)

-- | Filter out quotes
filterQuotes :: Timeline -> Timeline
filterQuotes = filter ((==Nothing) . view quoted)

-- | Get a list of tweets from a response, returning author, favorites, retweets, and content.
getTweets :: BS2.ByteString -> Either (ParseErrorBundle String Void) Timeline
getTweets = parse parseTweet "" . BS.unpack

-- | Display Timeline without color
displayTimeline :: Timeline -> String
displayTimeline (TweetEntity content _ u sn idTweet _ Nothing rts fave:rest) = concat [u
    , " ("
    , sn
    , ")"
    ,":\n    "
    ,fixNewline content
    ,"\n    "
    , "💜"
    -- , "♥ "
    ,show fave
    , " \61561  "
    -- ," ♺ "
    ,show rts
    , "  "
    , show idTweet
    ,"\n\n"
    ,displayTimeline rest]
displayTimeline (TweetEntity content _ u sn idTweet _ (Just q) rts fave:rest) = concat [u
    , " ("
    , sn
    , ")"
    , ":\n    "
    , fixNewline content
    , "\n    "
    , "💜"
    , show fave
    , " \61561  "
    , show rts
    , "  "
    , show idTweet
    , "\n    "
    , _name q
    , " ("
    , _screenName q
    , ")"
    , ": "
    , _text q
    , "\n\n"
    , displayTimeline rest]
displayTimeline [] = []

bird :: String
bird = toPlainBlue "🐦\n"

-- | Display Timeline in color
displayTimelineColor :: Timeline -> String
displayTimelineColor (TweetEntity content _ u sn idTweet _ Nothing rts fave:rest) = concat [toYellow u
    , " ("
    , sn
    , ")"
    , ":\n    "
    , fixNewline content
    , "\n    "
    , toRed "💜"
    , " "
    , show fave
    , toGreen " \61561  " -- ♺ "
    , show rts
    , "  "
    , toBlue (show idTweet)
    , "\n\n"
    , displayTimelineColor rest]
displayTimelineColor (TweetEntity content _ u sn idTweet _ (Just q) rts fave:rest) = concat [toYellow u
    , " ("
    , sn
    , ")"
    , ":\n    "
    , fixNewline content
    , "\n    "
    , toRed "💜"
    , " "
    , show fave
    , toGreen " \61561  "
    , show rts
    , "  "
    , toBlue (show idTweet)
    , "\n    "
    , toYellow $ _name q
    , " ("
    , _screenName q
    , ")"
    , ": "
    , _text q
    , "\n\n"
    , displayTimelineColor rest]
displayTimelineColor [] = []

-- | When displaying, newlines should include indentation.
fixNewline :: String -> String
fixNewline = replace "\n" "\n    "

-- | sort tweets by most successful
sortTweets :: Timeline -> Timeline
sortTweets = sortBy compareTweet
    where compareTweet (TweetEntity _ _ _ _ _ _ _ r1 f1) (TweetEntity _ _ _ _ _ _ _ r2 f2) = compare (2*r2 + f2) (2*r1 + f1)

-- | helper function to get the key as read from a file
keyLinePie :: String -> String
keyLinePie = takeWhile (/=':')

-- | Pick out a key value from a key
lineByKey :: BS.ByteString -> [(BS.ByteString, BS.ByteString)] -> BS.ByteString
lineByKey = snd .* head .* (filter . (fst -.* (==)))

-- | Filter a line of a file for only the actual data and no descriptors
filterLine :: String -> String
filterLine = reverse . takeWhile (not . (`elem` (" :" :: String))) . reverse

-- | Get pairs of "key" to search for and actual values
getConfigData :: FilePath -> IO [(BS.ByteString, BS.ByteString)]
getConfigData filepath = zip <$> keys <*> content
    where content = map (BS.pack . filterLine) . lines <$> file
          keys    = map (BS.pack . keyLinePie) . lines <$> file
          file    = readFile filepath
