{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Web.Tweet.Parser
import qualified Data.ByteString as BS

main :: IO ()
main = hspec $ do
    describe "parseTweet" $ do
        file <- runIO $ BS.readFile "test/data"
        parallel $ it "parses sample tweets" $ do
            parse parseTweet "" `shouldSucceedOn` file
