-- | Miscellaneous functions that don't fit the project directly
module Web.Tweet.Utils where

import qualified Data.ByteString.Char8 as BS
import Text.Megaparsec.String
import Text.Megaparsec
import Data.Monoid
import Web.Tweet.Types
import Control.Monad
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), char, (<>), string)

-- example text
-- Emma Watson, seins nus dans \u00abVanity Fair\u00bb, f\u00e9ministe ou hypocrite? \u27a1\ufe0f https:\/\/t.co\/MIPx1EpRSK https:\/\/t.co\/uSnLayoPi6
--"screen_name":"C_mrmt"
--toUnicode :: Parser String
--toUnicode "\u00e9" = 

displayTimelineColor :: Timeline -> String
displayTimelineColor ((user,content):rest) = ((show . yellow . text $ user) <> ":\n    " <> content <> "\n") <> (displayTimeline rest)
displayTimelineColor [] = []

displayTimeline :: Timeline -> String
displayTimeline ((user,content):rest) = (user <> ":\n    " <> content <> "\n") <> (displayTimeline rest) -- color should be configurable at least! 
displayTimeline [] = []

-- | Get tweets from a response, disgarding all but author and 
getTweets str = zip <$> (parse (filterStr "screen_name") "" str) <*> (parse (filterStr "text") "" str)

filterTl :: Parser Timeline
filterTl = zip <$> (filterStr "screen_name") <*> (filterStr "text")

filterStr :: String -> Parser [String]
filterStr str = (fmap (filter (/=""))) . many $
    filterTag str <|> (const "" <$> anyChar)

filterTag :: String -> Parser String
filterTag str = do
    string $ "\"" <> str <> "\""
    char ':'
    char '\"'
    want <- many $ noneOf "\""
    char '\"'
    pure want

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
