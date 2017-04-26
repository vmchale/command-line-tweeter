module Main where

import Criterion.Main
import Text.Megaparsec
import Web.Tweet.Parser
import Web.Tweet.Parser.FastParser
import qualified Data.ByteString as BS

main = do
    defaultMain [ env setupEnv $ \file ->
                  bgroup "parseTweet"
                      [ bench "226" $ whnf fun file ]
                , env setupEnv $ \file ->
                  bgroup "fastParser"
                      [ bench "226" $ whnf fast file ] ]
    where fun      = parse parseTweet ""
          fast     = fastParse
          setupEnv = BS.readFile "test/data"

