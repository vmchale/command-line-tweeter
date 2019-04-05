module Main where

import           Criterion.Main
import qualified Data.ByteString  as BS
import           Text.Megaparsec
import           Web.Tweet.Parser

setupEnv :: IO BS.ByteString
setupEnv = BS.readFile "test/data"

setupEnv' :: IO String
setupEnv' = readFile "test/data"

main :: IO ()
main =
    defaultMain [
              env setupEnv' $ \ file ->
              bgroup "handrolled parser"
                  [ bench "226" $ whnf (parse parseTweet) file ]
            ]
