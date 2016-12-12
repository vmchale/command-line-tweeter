-- | Provides IO action that parses command line options and tweets from stdin
module Web.Exec (exec) where

import Web.Tweet
import Options.Applicative
import qualified Data.ByteString.Char8 as BS
import Data.List.Split (chunksOf)

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
fromStdIn num filepath = do
    content <- fmap ((take num) . (map BS.pack) .  (chunksOf 140)) getContents
    sequence_ $ fmap (tweet filepath) content

-- | Data type for our program; just one optional argument for path to credential file.
data Program = Program { cred :: Maybe FilePath , tweets :: Maybe String }

-- | Executes program
select :: Program -> IO ()
select (Program Nothing (Just n)) = fromStdIn (read n) ".cred"
select (Program Nothing Nothing) = fromStdIn 4 ".cred"
select (Program (Just file) (Just n)) = fromStdIn (read n) file
select (Program (Just file) Nothing) = fromStdIn 4 file

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
