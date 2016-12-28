-- | Provides IO action that parses command line options and tweets from stdin
module Web.Exec (exec) where

import Web.Tweet
import Options.Applicative
import qualified Data.ByteString.Char8 as BS
import Data.List.Split (chunksOf)
import Control.Monad

-- | Data type for our program; just one optional argument for path to credential file.
data Program = Program { cred :: Maybe FilePath , tweets :: Maybe String, replyId :: Maybe String, replyHandles :: Maybe [String] }

-- | Executes parser
exec :: IO ()
exec = execParser opts >>= select
    where
        opts = info (helper <*> program)
            (fullDesc
            <> progDesc "Tweet from stdin!"
            <> header "clit - a Command Line Interface Tweeter")

-- | query twitter to post stdin
fromStdIn :: Int -> FilePath -> IO ()
fromStdIn = thread [] 0
    --do
    --content <- (take num) . (map BS.pack) .  (chunksOf 140) <$> getContents
    --sequence_ $ (flip tweet filepath) <$> content

-- | thread tweets together nicely. Takes a list of handles to reply to, plus the ID of the status you're replying to.
thread :: [String] -> Int -> Int -> FilePath -> IO ()
thread hs replyID num filepath = do
    let handleStr = concatMap (((++) " ") . ((++) "@")) hs
    content <- (take num) . (chunksOf (140-(length handleStr))) <$> getContents
    print $ urlString (Tweet { status = content !! 0, trimUser = True, handles = hs, replyID = pure 0})
    let f = (\str i -> (flip tweetData filepath) (Tweet { status = str, trimUser = True, handles = hs, replyID = Just i }))
    let initial = f (content !! 0)
    void $ foldr ((>=>) . f) initial content $ replyID

-- | Executes program
select :: Program -> IO ()
select (Program Nothing (Just n) Nothing Nothing) = fromStdIn (read n) ".cred"
select (Program Nothing Nothing Nothing Nothing) = fromStdIn 4 ".cred"
select (Program (Just file) (Just n) Nothing Nothing) = fromStdIn (read n) file
select (Program (Just file) Nothing Nothing Nothing) = fromStdIn 4 file
select (Program Nothing (Just n) (Just id) (Just handles)) = thread handles (read id) (read n) ".cred"
select (Program (Just file) (Just n) (Just id) (Just handles)) = thread handles (read id) (read n) file
select (Program (Just file) Nothing (Just id) (Just handles)) = thread handles (read id) 4 file
select (Program Nothing (Just n) (Just id) Nothing) = thread [] (read id) (read n) ".cred"
select (Program Nothing Nothing (Just id) Nothing) = thread [] (read id) 4 ".cred"
select (Program (Just file) (Just n) (Just id) Nothing) = thread [] (read id) (read n) file

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
