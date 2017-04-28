{-# LANGUAGE DeriveGeneric     #-}

module Web.Tweet.Parser.FastParser ( fastParse
                                   , FastTweet (..)
                                   ) where

import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
--import Data.Vector

data FastTweet = FastTweet
    { id :: !Int
    , text :: !T.Text
    , user :: User
    , quoted_status :: Maybe FastTweet
    , retweet_count :: !Int
    , favorite_count :: !Int
    } deriving (Generic, Eq, Show)

data User = User { name        :: !T.Text
                 , screen_name :: !T.Text } 
                 deriving (Generic, Eq, Show)

instance FromJSON FastTweet

instance FromJSON User

fastParse :: BS.ByteString -> Either String [FastTweet] -- (Vector FastTweet)
fastParse = eitherDecode . BSL.fromStrict
