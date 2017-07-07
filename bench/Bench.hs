module Main where

import           Criterion.Main
import qualified Data.ByteString             as BS
import           Web.Tweet.Parser.FastParser
import Web.Tweet.Parser
import Text.Megaparsec

setupEnv = BS.readFile "test/data"
setupEnv' = readFile "test/data"

fast = fmap (fmap fromFast) . fastParse

main = do
    defaultMain [
                env setupEnv $ \ ~file ->
                bgroup "aeson parser"
                      [ bench "226" $ whnf fast file ]
                , env setupEnv' $ \ ~file ->
                  bgroup "handrolled parser"
                      [ bench "226" $ whnf (parse parseTweet) file ]
                ]
