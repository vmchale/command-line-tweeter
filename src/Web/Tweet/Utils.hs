-- | Miscellaneous functions that don't fit the project directly
module Web.Tweet.Utils where

import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import Data.Monoid
import Web.Tweet.Types
import Control.Lens.Tuple
import Control.Lens hiding (noneOf)
import Data.Function
import Web.Tweet.Utils.Colors
import Data.List.Extra
import Web.Tweet.Parser
import Text.Megaparsec

hits :: Timeline -> Timeline
hits = sortTweets . filterRTs

filterRTs :: Timeline -> Timeline
filterRTs = filter ((/="RT @") . take 4 . (view text))

-- | Get a list of tweets from a response, returning author, favorites, retweets, and content. 
getTweets :: String -> Either (ParseError Char Dec) Timeline
getTweets = parse parseTweet "" 

-- | Display Timeline without color
displayTimeline :: Timeline -> String
displayTimeline ((TweetEntity content user _ _ Nothing fave rts):rest) = (user <> ":\n    " <> (fixNewline content)) <> "\n    " <> "♥ " <> (show fave) <> " ♺ " <> (show rts) <> "\n\n" <> (displayTimeline rest) 
displayTimeline ((TweetEntity content user _ _ (Just quoted) fave rts):rest) = (user <> ":\n    " <> (fixNewline content)) <> "\n    " <> "♥ " <> (show fave) <> " ♺ " <> (show rts) <> "\n    " <> (_name quoted) <> ": " <> (_text quoted) <> "\n\n" <> (displayTimeline rest) 
displayTimeline [] = []

-- | Display Timeline in color
displayTimelineColor :: Timeline -> String
displayTimelineColor ((TweetEntity content user _ _ Nothing fave rts):rest) = ((toYellow user) <> ":\n    " <> (fixNewline content)) <> "\n    " <> (toRed "♥ ") <> (show fave) <> (toGreen " ♺ ") <> (show rts) <> "\n\n" <> (displayTimelineColor rest) 
displayTimelineColor ((TweetEntity content user _ _ (Just quoted) fave rts):rest) = ((toYellow user) <> ":\n    " <> (fixNewline content)) <> "\n    " <> (toRed "♥ ") <> (show fave) <> (toGreen " ♺ ") <> (show rts) <> "\n    " <> (toYellow $ _name quoted) <> ": " <> (_text quoted) <> "\n\n" <> (displayTimelineColor rest) 
displayTimelineColor [] = []

fixNewline :: String -> String
fixNewline = replace "\n" "\n    "

-- TODO add feature to filter out quotes etc. Or make it a sub-thing?
sortTweets :: Timeline -> Timeline
sortTweets = sortBy compareTweet
    where compareTweet (TweetEntity _ _ _ _ _ f1 r1) (TweetEntity _ _ _ _ _ f2 r2) = compare (2*r2 + f2) (2*r1 + f1)

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

-- `FIXME` 
parseDMs = zip <$> (extractEvery 2 <$> filterStr "screen_name") <*> (filterStr "text")
    where extractEvery n = map snd . filter ((== n) . fst) . zip (cycle [1..n])
