{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Web.Tweet.Parser
import qualified Data.ByteString as BS
--
import Web.Tweet.Parser.FastParser

-- TODO make sure it's the right number of tweets as well
main :: IO ()
main = hspec $ do
    describe "parseTweet" $ do
        file <- runIO $ BS.readFile "test/data"
        parallel $ it "parses sample tweets" $ do
            parse parseTweet "" `shouldSucceedOn` file
            {--
    describe "fastParse" $ do
        file <- runIO $ BS.readFile "test/data"
        parallel $ it "parses sample tweets wrong" $ do
            fastParse "" `shouldBe` Left "some error idk"
            --}
