module Main where

import Criterion.Main
import Text.Megaparsec
import Web.Tweet.Parser

fun = parse parseTweet ""

main = do
    file <- readFile "test/data"
    defaultMain [ bgroup "parseTweet"
                      [ bench "20" $ whnf fun file ]
                ]
