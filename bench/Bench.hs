module Main where

import Criterion.Main
import Text.Megaparsec
import Web.Tweet.Parser
import Web.Tweet.Parser.FastParser
import Data.ByteString as BS

fun = parse parseTweet ""

fast = fastParse

main = do
    file <- BS.readFile "test/data"
    defaultMain [ bgroup "parseTweet"
                      [ bench "20" $ whnf fun file ]
                {--, bgroup "fastParser"
                      [ bench "20" $ whnf fast file ]--}
                ]
