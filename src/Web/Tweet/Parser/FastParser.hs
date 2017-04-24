{-# LANGUAGE DeriveGeneric     #-}

module Web.Tweet.Parser.FastParser ( fastParse
                                   , FastTweet (..)
                                   ) where

import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

data FastTweet = FastTweet
    { text :: !T.Text
    , name :: !T.Text
    , screenName :: !T.Text
    , tweetId :: !Int
    , quoted_status :: Maybe FastTweet
    , retweets :: !Int
    , favorites :: !Int
    } deriving (Generic, Eq, Show)

instance FromJSON FastTweet

fastParse :: BS.ByteString -> Either String [FastTweet]
fastParse = eitherDecode . BSL.fromStrict
