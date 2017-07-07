{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString             as BS
import           Test.Hspec
import           Web.Tweet.Parser.FastParser
import Web.Tweet.Sign
import Data.Monoid
import System.Environment
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "fastParse" $ do
        file <- runIO $ BS.readFile "test/data"
        config <- runIO $ (<> "/.cred") <$> getEnv "HOME"
        configToml <- runIO $ (<> "/.cred.toml") <$> getEnv "HOME"
        parallel $ it "parses sample tweets wrong" $ do
            fastParse "" `shouldBe` Left "Error in $: not enough input"
        parallel $ it "parses a config file the same way with the toml parser" $
            ((==) <$> mkConfigToml configToml <*> mkConfig config) >>= (`shouldBe` True)
