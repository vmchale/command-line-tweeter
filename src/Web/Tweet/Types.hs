{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass  #-}

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
    } deriving (Generic, Default, Eq, Show)

-- | Stores data like (name, text, favoriteCount, retweetCount)
type Timeline = [TweetEntity]

-- | Contains an 'OAuth' and a 'Credential'; encapsulates everything needed to sign a request.
type Config = (OAuth, Credential)

-- | Lens for `Tweet` accessing the `status` field.
status :: Lens' Tweet String
status f tweet@Tweet { _status = str } = fmap (\str' -> tweet { _status = str'}) (f str)

-- | Lens for `Tweet` accessing the `handles` field.
handles :: Lens' Tweet [String]
handles f tweet@Tweet { _handles = hs } = fmap (\hs' -> tweet { _handles = hs'}) (f hs)

-- | Lens for `Tweet` accessing the `_replyID` field.
replyID :: Lens' Tweet (Maybe Int)
replyID f tweet@Tweet { _replyID = reply } = fmap (\reply' -> tweet { _replyID = reply'}) (f reply)

-- | Lens for `TweetEntity` accessing the `_text` field.
text :: Lens' TweetEntity String
text f tweet@TweetEntity { _text = txt } = fmap (\txt' -> tweet { _text = txt'}) (f txt)

-- | Lens for `TweetEntity` accessing the `_name` field.
name :: Lens' TweetEntity String
name f tweet@TweetEntity { _name = nam } = fmap (\nam' -> tweet { _name = nam'}) (f nam)

-- | Lens for `TweetEntity` accessing the `_screenName` field.
screenName :: Lens' TweetEntity String
screenName f tweet@TweetEntity { _screenName = scr } = fmap (\scr' -> tweet { _screenName = scr'}) (f scr)

-- | Lens for `TweetEntity` accessing the `_tweetId` field.
tweetId :: Lens' TweetEntity Int 
tweetId f tweet@TweetEntity { _tweetId = tw } = fmap (\tw' -> tweet { _tweetId = tw'}) (f tw)

-- | Lens for `TweetEntity` accessing the `_quoted` field.
quoted :: Lens' TweetEntity (Maybe TweetEntity) 
quoted f tweet@TweetEntity { _quoted = quot } = fmap (\quot' -> tweet { _quoted = quot'}) (f quot)

-- | Lens for `TweetEntity` accessing the `_retweets` field.
retweets :: Lens' TweetEntity Int 
retweets f tweet@TweetEntity { _retweets = rts } = fmap (\rts' -> tweet { _retweets = rts'}) (f rts)

-- | Lens for `TweetEntity` accessing the `_favorites` field.
favorites :: Lens' TweetEntity Int 
favorites f tweet@TweetEntity { _favorites = fav } = fmap (\fav' -> tweet { _favorites = fav'}) (f fav)
