{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Web.Tweet.Parser
import Web.Tweet.Utils

main :: IO ()
main = hspec $ do
    describe "parseTweet" $ do
        file <- runIO $ readFile "test/data"
        parallel $ it "parses sample tweets" $ do
            parse parseTweet "" `shouldSucceedOn` file
