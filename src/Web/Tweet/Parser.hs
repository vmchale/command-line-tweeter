-- FIXME make this module available under cabal file
module Web.Tweet.Parser where

import Text.Megaparsec.String
import Text.Megaparsec.Lexer as L
import Text.Megaparsec
import Web.Tweet.Types
import Data.Monoid
import Data.Maybe
import Control.Monad

-- | Parse some number of tweets
parseTweet :: Parser Timeline
parseTweet = many (try getData <|> (const (TweetEntity "" "" "" 0 Nothing 0 0) <$> eof))

-- | Parse a single tweet's: name, text, fave count, retweet count
getData :: Parser TweetEntity
getData = do
    id <- read <$> filterStr "id" 
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
    want <- many $ noneOf forbidden <|> specialChar '\"' <|> specialChar '/' <|> newlineChar <|> unicodeChar -- specialChar 'u'
    pure want

-- | Parse a newline
newlineChar :: Parser Char
newlineChar = do
    string "\\n"
    pure '\n'

-- | Parser for unicode; twitter will give us something like "/u320a"
unicodeChar :: Parser Char
unicodeChar = do
    string "\\u"
    num <- fromHex . filterEmoji <$> count 4 anyChar
    pure . toEnum . fromIntegral $ num

-- | helper function to ignore emoji
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
