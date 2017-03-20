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
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), char, (<>), string)
import Control.Lens.Tuple
import Control.Lens hiding (noneOf)
import Data.Function

-- `FIXME` 
parseDMs = zip <$> (extractEvery 2 <$> filterStr "screen_name") <*> (filterStr "text")
    where extractEvery n = map snd . filter ((== n) . fst) . zip (cycle [1..n])

-- | Display Timeline without color
displayTimeline :: Timeline -> String
displayTimeline ((user,content,fave,rts):rest) = (user <> ":\n    " <> content) <> "\n    " <> "♥ " {-- ♡💛--} <> fave <> " ♺ " <> rts <> "\n\n" <> (displayTimeline rest) -- 
displayTimeline [] = []

-- | Display Timeline in color
displayTimelineColor :: Timeline -> String
displayTimelineColor ((user,content,fave,rts):rest) = ((show . yellow . text $ user) <> ":\n    " <> content) <> "\n    " <> (show . red . text $ "♥ ") {-- ♡💛--} <> fave <> (show . green . text $ " ♺ ") <> rts <> "\n\n" <> (displayTimelineColor rest) -- 
displayTimelineColor [] = []

-- | Get a list of tweets from a response, returning author, favorites, retweets, and content. 
getTweets = parse parseTweet "" 

-- | Parse some number of tweets
parseTweet :: Parser Timeline
parseTweet = many (try getData <|> (const ("","","","") <$> eof))

hits = sortFaves . filterRTs

filterRTs :: Timeline -> Timeline
filterRTs = filter ((/="RT") . take 2 . (view _2))

sortFaves :: Timeline -> Timeline
sortFaves = sortBy compareFavorites
    where compareFavorites = on compare ((read :: String -> Int) . (view _3))

-- | Parse a single tweet's: name, text, fave count, retweet count
getData :: Parser (String, String, String, String)
getData = do
    text <- filterStr "text"
    skipMentions
    --userMentions <- filterStr "user_mentions"
    --name <- if userMentions == "[]" then filterStr "name" else filterStr "name" >> filterStr "name" -- FIXME fix this to read number of userMentions
    name <- filterStr "name"
    isQuote <- filterStr "is_quote_status"
    case isQuote of
        "false" -> do
            rts <- filterStr "retweet_count"
            faves <- filterStr "favorite_count"
            pure (name, text, faves, rts)
        "true" -> do
            skipMentions
            rts <- filterStr "retweet_count" >> filterStr "retweet_count"
            faves <- filterStr "favorite_count"
            pure (name, text, faves, rts)

-- TODO make it work when user names include ]
skipInsideBrackets :: Parser ()
skipInsideBrackets =void (between (char '[') (char ']') $ many (skipInsideBrackets <|> void (noneOf "[]")))

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
    want <- many $ noneOf forbidden <|> specialChar '\"' <|> specialChar '/' <|> newlineChar <|> unicodeChar -- specialChar 'u'
    pure want

-- | Parse a newline
newlineChar :: Parser Char
newlineChar = do
    string "\\n"
    pure '\n'

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
