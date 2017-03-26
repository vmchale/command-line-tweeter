module Main where

import Criterion.Main
import Text.Megaparsec
import Web.Tweet.Parser
import Data.ByteString as BS

fun = parse parseTweet ""

main = do
    file <- BS.readFile "test/data"
    defaultMain [ bgroup "parseTweet"
                      [ bench "20" $ whnf fun file ]
                ]
