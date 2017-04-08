{-# LANGUAGE DeriveGeneric     #-}

module Web.Tweet.Parser.FastParser ( fastParse
                                   , FastTweet (..)
                                   ) where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

data FastTweet = FastTweet
    { text :: String 
    , name :: String
    , screenName :: String
    , tweetId :: Int
    -- , quoted :: Maybe FastTweet
    , retweets :: Int
    , favorites :: Int
    } deriving (Generic, Eq, Show)

instance FromJSON FastTweet

fastParse :: BS.ByteString -> Either String [FastTweet]
fastParse = eitherDecode . BSL.fromStrict

-- consider doing something for ffi? learning and whatnot.
