-- | Miscellaneous functions that don't fit the project directly
module Web.Tweet.Utils where

import qualified Data.ByteString.Char8 as BS
import Text.Megaparsec.String
import Text.Megaparsec
import Data.Monoid
import Data.Maybe
import Web.Tweet.Types
import Control.Monad
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), char, (<>), string)
import Control.Lens.Tuple

-- example text
-- Emma Watson, seins nus dans \u00abVanity Fair\u00bb, f\u00e9ministe ou hypocrite? \u27a1\ufe0f https:\/\/t.co\/MIPx1EpRSK https:\/\/t.co\/uSnLayoPi6
--"screen_name":"C_mrmt"
--toUnicode :: Parser String
--toUnicode "\u00e9" = 

parseDMs = zip <$> (extractEvery 2 <$> filterStr "screen_name") <*> (filterStr "text")
    where extractEvery n = map snd . filter ((== n) . fst) . zip (cycle [1..n])

displayTimeline :: Timeline -> String
displayTimeline ((user,content,fave,rts):rest) = (user <> ":\n    " <> content) <> "\n    " <> "♡ " {-- 💛--} <> fave <> "\n" <> (displayTimeline rest)
displayTimeline [] = []

displayTimelineColor :: Timeline -> String
displayTimelineColor ((user,content,fave,rts):rest) = ((show . yellow . text $ user) <> ":\n    " <> content) <> "\n    " <> (show . red . text $ "♥ ") {-- ♡💛--} <> fave <> (show . green . text $ " ♺ ") <> rts <> "\n" <> (displayTimelineColor rest) -- 
displayTimelineColor [] = []

-- | Get tweets from a response, disgarding all but author, favorites, retweets, and content
getTweets = parse parseTweet "" 

parseTweet :: Parser Timeline
parseTweet = many (try getData <|> (const ("","","","") <$> eof))

getData :: Parser (String, String, String, String)
getData = do
    text <- filterStr "text"
    userMentions <- filterStr "user_mentions"
    name <- if userMentions == "[]" then filterStr "name" else filterStr "name" >> filterStr "name"
    isQuote <- filterStr "is_quote_status"
    case isQuote of
        "false" -> do
            rts <- filterStr "retweet_count"
            faves <- filterStr "favorite_count"
            pure (name, text, faves, rts)
        "true" -> do
            rts <- filterStr "retweet_count" >> filterStr "retweet_count"
            faves <- filterStr "favorite_count"
            pure (name, text, faves, rts)

filterStr :: String -> Parser String
filterStr str = do
    many $ try $ anyChar >> notFollowedBy (string ("\"" <> str <> "\":"))
    char ','
    filterTag str

filterTag :: String -> Parser String
filterTag str = do
    string $ "\"" <> str <> "\":"
    open <- optional $ char '\"'
    let forbidden = if (isJust open) then "\\\"" else "\\\","
    want <- many $ noneOf forbidden <|> specialChar '\"' <|> specialChar '/' <|> specialChar 'n' <|> specialChar 'u'
    pure want

specialChar :: Char -> Parser Char
specialChar c = do
    string $ "\\" ++ pure c
    if c /= '\n' then pure c else pure '\n'

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
