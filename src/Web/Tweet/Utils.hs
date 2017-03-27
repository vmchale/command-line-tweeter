-- | Miscellaneous functions that don't fit the project directly
module Web.Tweet.Utils where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as BS2
import Data.Char
import Data.List
import Web.Tweet.Types
import Control.Lens.Tuple
import Control.Lens hiding (noneOf)
import Data.Function
import Web.Tweet.Utils.Colors
import Data.List.Extra
import Web.Tweet.Parser
import Text.Megaparsec

-- | filter out retweets, and sort by most successful.
hits :: Timeline -> Timeline
hits = sortTweets . filterRTs 

-- | Filter out retweets
filterRTs :: Timeline -> Timeline
filterRTs = filter ((/="RT @") . take 4 . (view text))

-- | Filter out quotes
filterQuotes :: Timeline -> Timeline
filterQuotes = filter ((==Nothing) . (view quoted))

-- | Get a list of tweets from a response, returning author, favorites, retweets, and content. 
getTweets :: BS2.ByteString -> Either (ParseError Char Dec) Timeline
getTweets = parse parseTweet "" 

-- | Display Timeline without color
displayTimeline :: Timeline -> String
displayTimeline ((TweetEntity content user screenName idTweet Nothing rts fave):rest) = concat [user
    , " ("
    , screenName
    , ")"
    ,":\n    " 
    ,fixNewline content 
    ,"\n    " 
    ,"♥ " 
    ,show fave 
    ," ♺ " 
    ,show rts 
    , "  "
    , show idTweet
    ,"\n\n" 
    ,displayTimeline rest]
displayTimeline ((TweetEntity content user screenName idTweet (Just quoted) rts fave):rest) = concat [user 
    , " ("
    , screenName
    , ")"
    , ":\n    " 
    , fixNewline content 
    , "\n    " 
    , "♥ " 
    , show fave 
    , " ♺ " 
    , show rts 
    , "  "
    , show idTweet
    , "\n    " 
    , _name quoted 
    , ": " 
    , _text quoted 
    , "\n\n" 
    , displayTimeline rest]
displayTimeline [] = []

-- | Display Timeline in color
displayTimelineColor :: Timeline -> String
displayTimelineColor ((TweetEntity content user screenName idTweet Nothing rts fave):rest) = concat [toYellow user 
    , " ("
    , screenName
    , ")"
    , ":\n    " 
    , fixNewline content
    , "\n    " 
    , toRed "♥ " 
    , show fave 
    , toGreen " ♺ " 
    , show rts 
    , "  "
    , toBlue (show idTweet)
    , "\n\n" 
    , displayTimelineColor rest]
displayTimelineColor ((TweetEntity content user screenName  idTweet (Just quoted) rts fave):rest) = concat [toYellow user 
    , " ("
    , screenName
    , ")"
    , ":\n    " 
    , fixNewline content 
    , "\n    " 
    , toRed "♥ " 
    , show fave 
    , toGreen " ♺ " 
    , show rts 
    , "  "
    , toBlue (show idTweet)
    , "\n    " 
    , toYellow $ _name quoted 
    , ": " 
    , _text quoted 
    , "\n\n" 
    , displayTimelineColor rest]
displayTimelineColor [] = []

-- | When displaying, newlines should include indentation.
fixNewline :: String -> String
fixNewline = replace "\n" "\n    "

-- | sort tweets by most successful
sortTweets :: Timeline -> Timeline
sortTweets = sortBy compareTweet
    where compareTweet (TweetEntity _ _ _ _ _ r1 f1) (TweetEntity _ _ _ _ _ r2 f2) = compare (2*r2 + f2) (2*r1 + f1)

-- | helper function to get the key as read from a file
keyLinePie :: String -> String
keyLinePie = takeWhile (/=':')

-- | Pick out a key value from a key
lineByKey :: BS.ByteString -> [(BS.ByteString, BS.ByteString)] -> BS.ByteString
lineByKey key = snd . head . (filter (\i -> fst i == key))

-- | Filter a line of a file for only the actual data and no descriptors
filterLine :: String -> String
filterLine = reverse . (takeWhile (not . (`elem` (" :" :: String)))) . reverse

-- | Get pairs of "key" to search for and actual values
getConfigData :: FilePath -> IO [(BS.ByteString, BS.ByteString)]
getConfigData filepath = zip <$> keys <*> content
    where content = (map (BS.pack . filterLine)) . lines <$> file
          keys    = (map (BS.pack . keyLinePie)) . lines <$> file
          file    = readFile filepath
