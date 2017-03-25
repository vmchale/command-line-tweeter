-- | Miscellaneous functions that don't fit the project directly
module Web.Tweet.Utils where

import qualified Data.ByteString.Char8 as BS
import Text.Megaparsec.String
import Text.Megaparsec.Lexer as L
import Text.Megaparsec
import Data.Char
import Data.List
import Data.Monoid
import Data.Maybe
import Web.Tweet.Types
import Control.Monad
import Control.Lens.Tuple
import Control.Lens hiding (noneOf)
import Data.Function
import Web.Tweet.Utils.Colors

-- `FIXME` 
parseDMs = zip <$> (extractEvery 2 <$> filterStr "screen_name") <*> (filterStr "text")
    where extractEvery n = map snd . filter ((== n) . fst) . zip (cycle [1..n])

-- | Display Timeline without color
displayTimeline :: Timeline -> String
displayTimeline ((TweetEntity content user _ _ Nothing fave rts):rest) = (user <> ":\n    " <> content) <> "\n    " <> "♥ " <> (show fave) <> " ♺ " <> (show rts) <> "\n\n" <> (displayTimeline rest) 
displayTimeline ((TweetEntity content user _ _ (Just quoted) fave rts):rest) = (user <> ":\n    " <> content) <> "\n    " <> "♥ " <> (show fave) <> " ♺ " <> (show rts) <> "\n    " <> (_name quoted) <> ": " <> (_text quoted) <> "\n\n" <> (displayTimeline rest) 
displayTimeline [] = []

-- | Display Timeline in color
displayTimelineColor :: Timeline -> String
displayTimelineColor ((TweetEntity content user _ _ Nothing fave rts):rest) = ((toYellow user) <> ":\n    " <> content) <> "\n    " <> (toRed "♥ ") <> (show fave) <> (toGreen " ♺ ") <> (show rts) <> "\n\n" <> (displayTimelineColor rest) 
displayTimelineColor ((TweetEntity content user _ _ (Just quoted) fave rts):rest) = ((toYellow user) <> ":\n    " <> content) <> "\n    " <> (toRed "♥ ") <> (show fave) <> (toGreen " ♺ ") <> (show rts) <> "\n    " <> (toYellow $ _name quoted) <> ": " <> (_text quoted) <> "\n\n" <> (displayTimelineColor rest) 
displayTimelineColor [] = []

-- | Get a list of tweets from a response, returning author, favorites, retweets, and content. 
getTweets = parse parseTweet "" 

-- TODO add feature to filter out quotes etc. Or make it a sub-thing?
sortTweets :: Timeline -> Timeline
sortTweets = sortBy compareTweet
    where compareTweet (TweetEntity _ _ _ _ _ f1 r1) (TweetEntity _ _ _ _ _ f2 r2) = compare (2*r2 + f2) (2*r1 + f1)

-- | Parse some number of tweets
parseTweet :: Parser Timeline
parseTweet = many (try getData <|> (const (TweetEntity "" "" "" 0 Nothing 0 0) <$> eof))

hits = sortTweets . filterRTs

filterRTs :: Timeline -> Timeline
filterRTs = filter ((/="RT @") . take 4 . (view text))

-- | Parse a single tweet's: name, text, fave count, retweet count
getData :: Parser TweetEntity
getData = do
    id <- read <$> filterStr "id" -- FIXME check it doesn't drop anything
    text <- filterStr "text"
    skipMentions
    name <- filterStr "name"
    screenName <- filterStr "screen_name"
    isQuote <- filterStr "is_quote_status"
    case isQuote of
        "false" -> do
            rts <- read <$> filterStr "retweet_count"
            faves <- read <$> filterStr "favorite_count"
            pure (TweetEntity text name screenName id Nothing rts faves)
        "true" -> do
            idQuoted <- read <$> filterStr "id"
            textQuoted <- filterStr "text"
            skipMentions
            nameQuoted <- filterStr "name"
            screenNameQuoted <- filterStr "screen_name"
            rtsQuoted <- read <$> filterStr "retweet_count"
            favesQuoted <- read <$> filterStr "favorite_count"
            rts <- read <$> filterStr "retweet_count"
            faves <- read <$> filterStr "favorite_count"
            pure $ TweetEntity text name screenName id (Just (TweetEntity textQuoted nameQuoted screenNameQuoted idQuoted Nothing rtsQuoted favesQuoted)) rts faves

-- TODO make it work when user names include ]
skipInsideBrackets :: Parser ()
skipInsideBrackets = void (between (char '[') (char ']') $ many (skipInsideBrackets <|> void (noneOf "[]")))

skipMentions :: Parser ()
skipMentions = do
    many $ try $ anyChar >> notFollowedBy (string ("\"user_mentions\":"))
    char ','
    string "\"user_mentions\":"
    skipInsideBrackets --between (char '[') (char ']') (many $ anyChar)
    pure ()

-- | Throw out input until we get to a relevant tag.
filterStr :: String -> Parser String
filterStr str = do
    many $ try $ anyChar >> notFollowedBy (string ("\"" <> str <> "\":"))
    char ','
    filterTag str

-- | Parse a field given its tag
filterTag :: String -> Parser String
filterTag str = do
    string $ "\"" <> str <> "\":"
    open <- optional $ char '\"'
    let forbidden = if (isJust open) then "\\\"" else "\\\","
    want <- (fmap concat) . many $ (pure <$> noneOf forbidden) <|> (pure <$> specialChar '\"') <|> (pure <$> specialChar '/') <|> newlineChar <|> (pure <$> unicodeChar) -- specialChar 'u'
    pure want

-- | Parse a newline
newlineChar :: Parser String
newlineChar = do
    string "\\n"
    pure "\n    "

-- TODO
--emoji :: Parser Char
--emoji = do
--    string "\\u"
    --d83d dc8c 💌

-- | Parser for unicode; twitter will give us something like "/u320a"
unicodeChar :: Parser Char
unicodeChar = do
    string "\\u"
    num <- fromHex . filterEmoji <$> count 4 anyChar
    pure . toEnum . fromIntegral $ num

-- | ignore emoji 
filterEmoji str = if head str == 'd' then "FFFD" else str

-- | Parse escaped characters
specialChar :: Char -> Parser Char
specialChar c = do
    string $ "\\" ++ pure c
    pure c

-- | Convert a string of four hexadecimal digits to an integer.
fromHex :: String -> Integer
fromHex = fromRight . (parse (L.hexadecimal :: Parser Integer) "")
    where fromRight (Right a) = a

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
