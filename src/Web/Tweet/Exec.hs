-- | Provides IO action that parses command line options and tweets from stdin
module Web.Tweet.Exec ( exec
                      , Program (Program)) where

import Web.Tweet
import Options.Applicative
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Data.Foldable (fold)

-- | Data type for our program: one optional path to a credential file, (optionally) the number of tweets to make, the id of the status you're replying to, and a list of users you wish to mention.
data Program = Program { cred :: Maybe FilePath , tweets :: Maybe String, replyId :: Maybe String, replyHandles :: Maybe [String] }

-- | query twitter to post stdin with no fancy options
fromStdIn :: Int -> FilePath -> IO ()
fromStdIn = thread [] Nothing

-- | Executes parser
exec :: IO ()
exec = execParser opts >>= select
    where
        opts = info (helper <*> program)
            (fullDesc
            <> progDesc "Tweet from stdin!"
            <> header "clit - a Command Line Interface Tweeter")

-- | Executes program
select :: Program -> IO ()
select (Program Nothing (Just n) Nothing Nothing) = fromStdIn (read n) ".cred"
select (Program Nothing Nothing Nothing Nothing) = fromStdIn 4 ".cred"
select (Program (Just file) (Just n) Nothing Nothing) = fromStdIn (read n) file
select (Program (Just file) Nothing Nothing Nothing) = fromStdIn 4 file
select (Program Nothing (Just n) (Just id) (Just handles)) = thread handles (read id) (read n) ".cred"
select (Program (Just file) (Just n) (Just id) (Just handles)) = thread handles (pure . read $ id) (read n) file
select (Program (Just file) Nothing (Just id) (Just handles)) = thread handles (pure . read $ id) 4 file
select (Program Nothing (Just n) (Just id) Nothing) = thread [] (pure . read $ id) (read n) ".cred"
select (Program Nothing Nothing (Just id) Nothing) = thread [] (pure . read $ id) 4 ".cred"
select (Program Nothing Nothing (Just id) (Just handles)) = thread handles (pure . read $ id) 4 ".cred"
select (Program (Just file) (Just n) (Just id) Nothing) = thread [] (pure . read $ id) (read n) file

-- | Parser to return a program datatype
program :: Parser Program
program = Program
    <$> (optional $ strOption
        (long "cred"
        <> short 'c'
        <> metavar "CREDENTIALS"
        <> help "path to credentials"))
    <*> (optional $ strOption
        (long "tweets"
        <> short 't'
        <> metavar "NUM"
        <> help "Number of tweets in a row, default 4"))
    <*> (optional $ strOption
        (long "reply"
        <> short 'r'
        <> help "id of status to reply to - be sure to include their handle, e.g. @my_build_errors"))
    <*> (optional $ some $ argument str
        (metavar "HANDLE1"
        <> help "handles to include in replies"))
