{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Exports the `Tweet` type, a datatype for building tweets easily
module Web.Tweet.Types where

import GHC.Generics
import Control.Lens
import Data.Default
import Web.Authenticate.OAuth

-- | Data type for our request: consists of the status text, whether to trium user information in the response, the handles to mention, and optionally the id of the status to reply to.
data Tweet = Tweet
    { _status   :: String
    , _handles  :: [String]
    , _replyID  :: Maybe Int
    } deriving (Generic, Default)

-- | Data type for tweets as they are returned
data TweetEntity = TweetEntity
    { _text :: String
    , _name :: String
    , _screenName :: String
    , _tweetId :: Int
    , _quoted :: Maybe TweetEntity
    , _retweets :: Int
    , _favorites :: Int
    } deriving (Generic, Default)

-- | Stores data like (name, text, favoriteCount, retweetCount)
type Timeline = [TweetEntity]

-- | Contains an 'OAuth' and a 'Credential'; encapsulates everything needed to sign a request.
type Config = (OAuth, Credential)

makeLenses ''Tweet

makeLenses ''TweetEntity
