{-# LANGUAGE OverloadedStrings #-}

-- | Functions to sign HTTP requests with oAuth
module Web.Tweet.Sign ( signRequest
                      , signRequestMem
                      , mkConfig
                      , mkConfigToml
                      , oAuthMem
                      , credentialMem ) where

import           Data.ByteString        as BS
import           Data.HashMap.Lazy
import           Data.Monoid
import qualified Data.Text              as T
import           Data.Text.Encoding     (encodeUtf8)
import qualified Data.Text.IO           as TIO
import           Network.HTTP.Client
import           Prelude                hiding (lookup)
import           Text.Toml
import           Web.Authenticate.OAuth
import           Web.Tweet.Types
import           Web.Tweet.Utils

-- | Sign a request using your OAuth dev token, as stored in a config file.
-- Uses the IO monad because signatures require a timestamp
signRequest :: FilePath -> Request -> IO Request
signRequest = (. flip signRequestMem) . (>>=) . mkConfig

-- | Sign a request using a 'Config' object, avoiding the need to read token/key from file
signRequestMem :: Config -> Request -> IO Request
signRequestMem = uncurry signOAuth

-- | Create an OAuth api key from two ByteStrings.
oAuthMem :: BS.ByteString -- ^ API key
         -> BS.ByteString -- ^ API secret
         -> OAuth
oAuthMem key secret = newOAuth { oauthConsumerKey = key, oauthConsumerSecret = secret, oauthServerName = "api.twitter.com" }

credentialMem :: BS.ByteString -- ^ Token
              -> BS.ByteString -- ^ Token secret
              -> Credential
credentialMem = newCredential

-- | Create an OAuth api key from config data in a file
oAuth :: FilePath -> IO OAuth
oAuth filepath = do
    secret <- lineByKey "api-sec" <$> getConfigData filepath
    key <- lineByKey "api-key" <$> getConfigData filepath
    let url = "api.twitter.com"
    pure newOAuth { oauthConsumerKey = key , oauthConsumerSecret = secret , oauthServerName = url }

getKey :: HashMap T.Text Node -> T.Text -> BS.ByteString
getKey hm key = case lookup key hm of
    (Just (VString k)) -> encodeUtf8 k
    (Just _) -> error $ "Key: " <> T.unpack key <> " found in the config file, but it is not a string."
    Nothing -> error $ "Key: " <> T.unpack key <> " not found in config file."

mkConfigToml :: FilePath -> IO Config
mkConfigToml filepath = do
    t <- TIO.readFile filepath
    let hm = case parseTomlDoc ("failed to read .toml at: " <> filepath) t of
            Right tab -> tab
            Left e    -> error (show e)
        secret = getKey hm "api-sec"
        key = getKey hm "api-key"
        tok = getKey hm "tok"
        tokSecret = getKey hm "tok-sec"
        url = "api.twitter.com"
        o = newOAuth { oauthConsumerKey = key , oauthConsumerSecret = secret , oauthServerName = url }
        c = newCredential tok tokSecret
    pure (o, c)

-- | Given a filepath, parse the contents of the file and return a configuration.
mkConfig :: FilePath -> IO Config
mkConfig filepath = do
    o <- oAuth filepath
    c <- credential filepath
    pure (o, c)

-- | Create a new credential from a token and token secret
credential :: FilePath -> IO Credential
credential filepath = newCredential <$> token <*> secretToken
    where token       = lineByKey "tok" <$> getConfigData filepath
          secretToken = lineByKey "tok-sec" <$> getConfigData filepath
