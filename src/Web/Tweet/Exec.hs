-- | Provides IO action that parses command line options and tweetInputs from stdin
module Web.Tweet.Exec ( exec
                      , Program (..)
                      , Command (..)) where

import Web.Tweet
import Options.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Monad
import Data.Foldable (fold)
import Data.List hiding (delete)
import Data.Monoid hiding (getAll)
import System.Directory
import Data.Maybe
import Paths_tweet_hs
import Data.Version

-- | Data type for our program: one optional path to a credential file, (optionally) the number of tweetInputs to make, the id of the status you're replying to, and a list of users you wish to mention.
data Program = Program { subcommand :: Command , cred :: Maybe FilePath , color :: Bool }

-- | Data type for a command
-- TODO add boolean option to show ids alongside tweets
data Command = Timeline { count :: Maybe Int }
    | SendInput { tweetInputs :: Maybe Int, replyId :: Maybe String, replyHandles :: Maybe [String] }
    | Profile { count :: Maybe Int , screenNameMaybe :: Maybe String }
    | Mentions { count :: Maybe Int }
    | Markov { screenName :: String }
    | Send { tweets :: Maybe Int , replyId :: Maybe String , replyHandles :: Maybe [String] , userInput :: String }
    | Sort { screenName :: String , count :: Maybe Int }
    | Delete { twId :: Integer }
    | Fav { twId :: Integer }
    | Unfav { twId :: Integer }
    | Retweet { twId :: Integer }
    | Unretweet { twId :: Integer }
    | Follow { screenName :: String }
    | Unfollow { screenName :: String }
    | Block { screenName :: String }
    | Unblock { screenName :: String }
    | Dump { screenName :: String }

-- | query twitter to post stdin with no fancy options
fromStdIn :: Int -> FilePath -> IO ()
fromStdIn = threadStdIn [] Nothing

-- | Tweet string given to us, as parsed from the command line
fromCLI :: String -> Int -> FilePath -> IO ()
fromCLI str = thread str [] Nothing

-- | Threaded tweetInputs from stdIn
threadStdIn :: [String] -> Maybe Int -> Int -> FilePath -> IO ()
threadStdIn hs idNum num filepath = do
    contents <- getContents
    thread contents hs idNum num filepath

-- | Executes parser
exec :: IO ()
exec = execParser opts >>= select
    where
        versionInfo = infoOption ("tweet-hs version: " ++ showVersion version) (short 'v' <> long "version" <> help "Show version")
        opts        = info (helper <*> versionInfo <*> program)
            (fullDesc
            <> progDesc "Tweet and view tweets"
            <> header "clit - a Command Line Interface Tweeter")

-- | Executes program given parsed `Program`
select :: Program -> IO ()
select (Program com maybeFile color) = case maybeFile of
    (Just file) -> selectCommand com color file
    _ -> selectCommand com color =<< (++ "/.cred") <$> getHomeDirectory

-- | Executes subcommand given subcommand + filepath to configuration file
selectCommand :: Command -> Bool -> FilePath -> IO ()
selectCommand (Send maybeNum Nothing Nothing input) _ file = fromCLI input (fromMaybe 15 maybeNum) file
selectCommand (Send maybeNum (Just replyId) Nothing input) _ file = thread input [] (pure . read $ replyId) (fromMaybe 15 maybeNum) file
selectCommand (Send maybeNum Nothing (Just handles) input) _ file = thread input handles Nothing (fromMaybe 15 maybeNum) file
selectCommand (SendInput maybeNum Nothing Nothing) _ file  = fromStdIn (fromMaybe 15 maybeNum) file
selectCommand (SendInput maybeNum (Just replyId) (Just handles)) _ file = threadStdIn handles (pure . read $ replyId) (fromMaybe 15 maybeNum) file
selectCommand (SendInput maybeNum (Just replyId) Nothing) _ file = threadStdIn [] (pure . read $ replyId) (fromMaybe 15 maybeNum) file
selectCommand (SendInput maybeNum Nothing (Just handles)) _ file = threadStdIn handles Nothing (fromMaybe 15 maybeNum) file
selectCommand (Timeline maybeNum) color file = putStrLn =<< showTimeline (fromMaybe 11 maybeNum) color file
selectCommand (Mentions maybeNum) color file = putStrLn =<< showTweets color <$> mentions (fromMaybe 11 maybeNum) file
selectCommand (Profile maybeNum name) color file = putStrLn =<< showProfile (fromMaybe mempty name) (fromMaybe 11 maybeNum) color file
selectCommand (Sort name maybeNum) color file = putStrLn =<< showBest name (fromMaybe 11 maybeNum) color file
selectCommand (Markov name) _ file = do
    raw <- getMarkov name Nothing file
    appendFile (name ++ ".txt") (unlines raw)
    putStrLn $ "Written output to: " ++ name ++ ".txt"
selectCommand (Delete n) color file = do
    putStrLn "Deleted:\n"
    putStrLn =<< showTweets color <$> deleteTweetResponse n file
selectCommand (Fav n) color file = do
    putStrLn "Favorited:\n"
    putStrLn =<< showTweets color <$> favoriteTweetResponse n file
selectCommand (Unfav n) color file = do
    putStrLn "Unfavorited:\n"
    putStrLn =<< showTweets color <$> unfavoriteTweetResponse n file
selectCommand (Retweet n) color file = do
    putStrLn "Retweeted:\n"
    putStrLn =<< showTweets color <$> retweetTweetResponse n file
selectCommand (Unretweet n) color file = do
    putStrLn "Unretweeted:\n"
    putStrLn =<< showTweets color <$> unretweetTweetResponse n file
selectCommand (Follow screenName) _ file = do
    follow screenName file
    putStrLn ("..." ++ screenName ++ " followed successfully!")
selectCommand (Unfollow screenName) _ file = do
    unfollow screenName file
    putStrLn ("..." ++ screenName ++ " unfollowed successfully!")
selectCommand (Block screenName) color file = do
    block screenName file
    putStrLn ("..." ++ screenName ++ " blocked successfully")
selectCommand (Unblock screenName) color file = do
    unblock screenName file
    putStrLn ("..." ++ screenName ++ " unblocked successfully")
selectCommand (Dump screenName) color file = BSL.putStrLn =<< (getProfileRaw screenName 3200 file Nothing)

-- | Parser to return a program datatype
program :: Parser Program
program = Program
    <$> (hsubparser
        (command "send" (info tweet (progDesc "Send a tweet from the command-line"))
        <> command "input" (info tweetInput (progDesc "Send a tweet from stdIn"))
        <> command "view" (info timeline (progDesc "Get your timeline"))
        <> command "user" (info profile (progDesc "Get a user's profile"))
        <> command "markov" (info markov (progDesc "Grab tweets en masse."))
        <> command "hits" (info best (progDesc "View a user's top tweets."))
        <> command "del" (info delete (progDesc "Delete a tweet.")) -- TODO delete/favorite in bunches!
        <> command "fav" (info fav (progDesc "Favorite a tweet"))
        <> command "ufav" (info unfav (progDesc "Unfavorite a tweet"))
        <> command "urt" (info unrt (progDesc "Un-retweet a tweet"))
        <> command "rt" (info rt (progDesc "Retweet a tweet"))
        <> command "follow" (info fol (progDesc "Follow a user"))
        <> command "unfollow" (info unfol (progDesc "Unfollow a user"))
        <> command "dump" (info dump (progDesc "Dump tweets (for debugging)"))
        <> command "block" (info blockParser (progDesc "Block a user"))
        <> command "unblock" (info unblockParser (progDesc "Unblock a user"))
        <> command "mention" (info mentionsParser (progDesc "Fetch mentions"))))
    <*> (optional $ strOption
        (long "cred"
        <> short 'c'
        <> metavar "CREDENTIALS"
        <> help "path to credentials"))
    <*> switch
        (long "color"
        <> short 'l'
        <> help "Display timeline with colorized terminal output.")

-- | Parser for the view subcommand
timeline :: Parser Command
timeline = Timeline
    <$> (optional $ read <$> strOption
        (long "count"
        <> short 'n'
        <> metavar "NUM"
        <> help "number of tweetInputs to fetch, default 5"))

-- | Parser for the markov subcommand
markov :: Parser Command
markov = Markov <$> user

-- | Parser for the follow subcommand
fol :: Parser Command
fol = Follow <$> user

-- | Parser for the block subcommand
blockParser :: Parser Command
blockParser = Block <$> user

-- | Parser for the unblock subcommand
unblockParser :: Parser Command
unblockParser = Unblock <$> user

-- | Parser for the dump subcommand
dump :: Parser Command
dump = Dump <$> user

-- | Parser for the unfollow subcommand
unfol :: Parser Command
unfol = Unfollow <$> user

-- | Parse a user screen name
user :: Parser String
user = argument str
    (metavar "SCREEN_NAME"
    <> help "Screen name of user.")

-- | Parser for the del subcommand
delete :: Parser Command
delete = Delete <$> getInt

-- | Parser for the fav subcommand
fav :: Parser Command
fav = Fav <$> getInt

-- | Parser for the fav subcommand
unfav :: Parser Command
unfav = Unfav <$> getInt

-- | Parser for the fav subcommand
unrt :: Parser Command
unrt = Unretweet <$> getInt

-- | Parser for the fav subcommand
rt :: Parser Command
rt = Retweet <$> getInt

-- | Parser for the del subcommand
getInt :: Parser Integer
getInt = read <$> (argument str
    (metavar "TWEET_ID"
    <> help "ID of tweet"))

-- | Parser for the user subcommand
profile :: Parser Command
profile = Profile
    <$> (optional $ read <$> strOption
        (long "count"
        <> short 'n'
        <> metavar "NUM"
        <> help "Number of tweetInputs to fetch, default 12"))
    <*> optional user 

-- | Parser for the mention subcommand
mentionsParser :: Parser Command
mentionsParser = Mentions
    <$> (optional $ read <$> strOption
        (long "count"
        <> short 'n'
        <> metavar "NUM"
        <> help "Number of tweetInputs to fetch, default 12"))

-- | Parse best tweets
best :: Parser Command
best = Sort
    <$> argument str
        (metavar "SCREEN_NAME"
        <> help "Screen name of user you want to view.")
    <*> (optional $ read <$> strOption
        (long "count"
        <> short 'n'
        <> metavar "NUM"
        <> help "Number of tweetInputs to fetch, default 12"))

-- | Parser for the send subcommand
tweet :: Parser Command
tweet = Send
    <$> (optional $ read <$> strOption
        (long "tweets"
        <> short 't'
        <> metavar "NUM"
        <> help "Number of tweetInputs in a row, default 15"))
    <*> (optional $ strOption
        (long "reply"
        <> short 'r'
        <> help "id of status to reply to - be sure to include their handle, e.g. @my_build_errors"))
    <*> (optional (some $ strOption
        (long "handle"
        <> short 'h'
        <> metavar "HANDLE1"
        <> help "Handles to include in replies")))
    <*> (unwords <$> (some $ argument str
        (metavar "TEXT"
        <> help "text of tweet to be sent")))

-- | Parser for the input command
tweetInput :: Parser Command
tweetInput = SendInput
    <$> (optional $ read <$> strOption
        (long "tweets"
        <> short 't'
        <> metavar "NUM"
        <> help "Number of tweets in a row, default 15"))
    <*> (optional $ strOption
        (long "reply"
        <> short 'r'
        <> help "id of status to reply to - be sure to include their handle, e.g. @my_build_errors"))
    <*> (optional (some $ argument str
        (metavar "HANDLE1"
        <> help "handles to include in replies")))
