{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Tweet.Parser.FastParser ( fastParse
                                   , fromFast
                                   , FastTweet (..)
                                   ) where

import           Data.Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Text            (unpack)
import qualified Data.Text            as T
import           GHC.Generics
import           Web.Tweet.Types      hiding (name, text)

data FastTweet = FastTweet
    { idNum          :: !Int
    , text           :: !T.Text
    , user           :: User
    , quoted_status  :: Maybe FastTweet
    , retweet_count  :: !Int
    , favorite_count :: !Int
    } deriving (Generic, Eq, Show)

data User = User { name        :: !T.Text
                 , screen_name :: !T.Text }
                 deriving (Generic, Eq, Show)

instance FromJSON FastTweet

instance FromJSON User

fromFast :: FastTweet -> TweetEntity
fromFast FastTweet{..} = TweetEntity (unpack text) (unpack . name $ user) (unpack . screen_name $ user) idNum mempty (fmap fromFast quoted_status) retweet_count favorite_count

fastParse :: BS.ByteString -> Either String [FastTweet]
fastParse = eitherDecode . BSL.fromStrict
