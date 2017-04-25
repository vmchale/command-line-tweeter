{-# LANGUAGE OverloadedStrings #-}
-- FIXME make this module available under cabal file
-- | Module containing parsers for tweet and response data.
module Web.Tweet.Parser ( parseTweet
                        , getData ) where

import qualified Data.ByteString as BS
import Text.Megaparsec.ByteString
import Text.Megaparsec.Lexer as L
import Text.Megaparsec
import Web.Tweet.Types
import Data.Monoid
import qualified Data.Map as M
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
    screenName' <- filterStr "screen_name"
    isQuote <- filterStr "is_quote_status"
    case isQuote of
        "false" -> do
            rts <- read <$> filterStr "retweet_count"
            faves <- read <$> filterStr "favorite_count"
            pure (TweetEntity text name screenName' id Nothing rts faves)
        "true" -> do
            quoted <- parseQuoted
            rts <- read <$> filterStr "retweet_count"
            faves <- read <$> filterStr "favorite_count"
            pure $ TweetEntity text name screenName' id quoted rts faves

-- | Parse a the quoted tweet
parseQuoted :: Parser (Maybe TweetEntity)
parseQuoted = do
    optional (string ",\"quoted_status_id" >> filterStr "quoted_status_id_str") -- FIXME it's skipping too many? prob is when two are deleted in a row twitter just dives in to RTs
    contents <- optional $ string "\",\"quoted_status"
    case contents of
        (Just contents) -> pure <$> getData
        _ -> pure Nothing
    

-- | Skip a set of square brackets []
skipInsideBrackets :: Parser ()
skipInsideBrackets = void (between (char '[') (char ']') $ many (skipInsideBrackets <|> void (noneOf ("[]" :: String))))

-- | Skip user mentions field to avoid parsing the wrong name
skipMentions :: Parser ()
skipMentions = do
    many $ try $ anyChar >> notFollowedBy (string "\"user_mentions\":")
    char ','
    string "\"user_mentions\":"
    skipInsideBrackets
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
    let forbidden = if isJust open then ("\\\"" :: String) else ("\\\"," :: String)
    want <- many $ parseHTMLChar <|> noneOf forbidden <|> specialChar '\"' <|> specialChar '/' <|> newlineChar <|> unicodeChar -- TODO modify parsec to make this parallel?
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
    num <- fromHex . filterEmoji . BS.pack . map (fromIntegral . fromEnum) <$> count 4 anyChar
    pure . toEnum . fromIntegral $ num

-- | helper function to ignore emoji
filterEmoji str = if BS.head str == (fromIntegral . fromEnum $ 'd') then "FFFD" else str

-- | Parse HTML chars
parseHTMLChar :: Parser Char
parseHTMLChar = do
    char '&'
    innards <- many $ noneOf (";" :: String)
    char ';'
    pure . (\(Just a) -> a) $ M.lookup innards (M.fromList [("amp",'&'),("gt",'>'),("lt",'<')])


-- | Parse escaped characters
specialChar :: Char -> Parser Char
specialChar c = do
    string $ "\\" ++ pure c
    pure c

-- | Convert a string of four hexadecimal digits to an integer.
fromHex :: BS.ByteString -> Integer
fromHex = fromRight . (parse (L.hexadecimal :: Parser Integer) "")
    where fromRight (Right a) = a
