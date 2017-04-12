{-# LANGUAGE OverloadedStrings #-}

-- | Utils for working with the API
module Web.Tweet.Utils.API where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Web.Tweet.Types
import Data.Char
import Data.Maybe
import Web.Authenticate.OAuth
import Data.Text.Encoding
import Web.Tweet.Sign

-- | Make a GET request to twitter given a request string
getRequestMem :: String -> Config -> IO BSL.ByteString
getRequestMem urlStr config = do
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest urlStr
    request <- signRequestMem config $ initialRequest { method = "GET" }
    responseBS request manager

-- | Make a GET request to twitter given a request string
getRequest :: String -> FilePath -> IO BSL.ByteString
getRequest = (. getRequestMem) . (>>=) . mkConfig 

-- | Make a POST request to twitter given a request string
postRequest :: String -> FilePath -> IO BSL.ByteString
postRequest = (. postRequestMem) . (>>=) . mkConfig 

-- | Make a POST request to twitter given a request string
postRequestMem :: String -> Config -> IO BSL.ByteString
postRequestMem urlStr config = do
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest urlStr
    request <- signRequestMem config $ initialRequest { method = "POST" }
    responseBS request manager

-- | Return HTTP request's result as a bytestring
responseBS :: Request -> Manager -> IO BSL.ByteString
responseBS request manager = do
    response <- httpLbs request manager
    let code = statusCode $ responseStatus response
    putStr $ if (code == 200) then "" else "failed :(\n error code: " ++ (show code) ++ "\n"
    pure . responseBody $ response

-- | print output of a request and return status id as an `Int`. 
responseInt :: Request -> Manager -> IO Int
responseInt request manager = do
    response <- httpLbs request manager
    let code = statusCode $ responseStatus response
    putStrLn $ if (code == 200) then "POST succesful!" else "failed :(\n error code: " ++ (show code)
    return $ (read . (takeWhile (/=',')) . (drop 52)) (BSL.unpack $ responseBody response) -- TODO use the more general parser

-- | Convert a tweet to a percent-encoded url for querying an API
urlString :: Tweet -> String
urlString tweet = concat [ "?status="
                         , BS.unpack (tweetEncode tweet)
                         , "&trim_user="
                         , map toLower (show trim)
                         , (if isJust (_replyID tweet) then "&in_reply_to_status_id=" else "")
                         , reply ]
    where trim  = False
          reply = fromMaybe "" (show <$> _replyID tweet)

-- | Percent-encode a string
strEncode :: String -> String
strEncode = BS.unpack . paramEncode . encodeUtf8 . T.pack

-- | Percent-encode the status update so it's fit for a URL and UTF-encode it as well. 
tweetEncode :: Tweet -> BS.ByteString
tweetEncode tweet = paramEncode . encodeUtf8 $ handleStr `T.append` content
    where content   = T.pack . _status $ tweet
          handleStr = T.pack $ concatMap ((++ " ") . ((++) "@")) hs
          hs        = _handles tweet
