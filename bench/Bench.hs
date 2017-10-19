module Main where

import           Criterion.Main
import qualified Data.ByteString             as BS
import           Text.Megaparsec
import           Web.Tweet.Parser
import           Web.Tweet.Parser.FastParser

setupEnv :: IO BS.ByteString
setupEnv = BS.readFile "test/data"

setupEnv' :: IO String
setupEnv' = readFile "test/data"

main :: IO ()
main = do
    defaultMain [
                env setupEnv $ \ ~file ->
                bgroup "aeson parser"
                      [ bench "226" $ whnf fast file ]
                , env setupEnv' $ \ ~file ->
                  bgroup "handrolled parser"
                      [ bench "226" $ whnf (parse parseTweet) file ]
                ]
    where
        fast = fmap (fmap fromFast) . fastParse

