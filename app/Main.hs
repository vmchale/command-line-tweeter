module Main where

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Foldable              (traverse_)
import           Data.Maybe
import           Data.Version
import           Options.Applicative
import           Paths_tweet_hs
import           System.Directory
import           Web.Tweet

-- | Data type for our program: one optional path to a credential file, (optionally) the number of tweetInputs to make, the id of the status you're replying to, and a list of users you wish to mention.
data Program = Program { subcommand :: Command , cred :: Maybe FilePath , color :: Bool }

-- | Data type for a command
-- TODO add boolean option to show ids alongside tweets
data Command = Timeline { count :: Maybe Int }
    | SendInput { tweetInputs :: Maybe Int, replyId :: Maybe String, replyh :: Maybe [String] }
    | Profile { count :: Maybe Int , screenName'' :: Maybe String, withReplies :: Bool, withRetweets :: Bool }
    | Mentions { count :: Maybe Int }
    | Markov { screenName' :: String }
    | Send { tweets :: Maybe Int , replyId :: Maybe String , replyh :: Maybe [String] , userInput :: String }
    | Sort { screenName' :: String , count :: Maybe Int , includeReplies :: Bool }
    | Delete { twId :: Integer }
    | Fav { twId :: Integer }
    | Unfav { twId :: Integer }
    | Retweet { twId :: Integer }
    | Unretweet { twId :: Integer }
    | List { count :: Maybe Int , screenName' :: String }
    | Follow { screenName' :: String }
    | Unfollow { screenName' :: String }
    | Block { screenName' :: String }
    | Unblock { screenName' :: String }
    | Mute { screenName' :: String }
    | Unmute { screenName' :: String }
    | Dump { screenName' :: String }
    | Replies { screenName' :: String, twId :: Integer }
    | MuteReplies { screenName' :: String, twId :: Integer }
    | MuteMentions { screenName' :: String }

-- | query twitter to post stdin with no fancy options
fromStdIn :: Int -> FilePath -> IO ()
fromStdIn = threadStdIn [] Nothing

-- | Tweet string given to us, as parsed from the command line
fromCLI :: String -> Int -> FilePath -> IO ()
fromCLI s = thread s [] Nothing

-- | Threaded tweetInputs from stdIn
threadStdIn :: [String] -> Maybe Int -> Int -> FilePath -> IO ()
threadStdIn hs idNum num filepath = do
    contents <- getContents
    thread contents hs idNum num filepath

-- | Executes parser
main :: IO ()
main = putStrLn bird >> execParser opts >>= select
    where
        versionInfo = infoOption ("tweet-hs version: " ++ showVersion version) (short 'v' <> long "version" <> help "Show version")
        opts        = info (helper <*> versionInfo <*> program)
            (fullDesc
            <> progDesc "Tweet and view tweets"
            <> header "tweet - a Command Line Interface Tweeter")

-- | Executes program given parsed `Program`
select :: Program -> IO ()
select (Program com maybeFile c) = case maybeFile of
    (Just file) -> selectCommand com (not c) file
    _ -> selectCommand com (not c) =<< (++ "/.cred.toml") <$> getHomeDirectory

-- | Executes subcommand given subcommand + filepath to configuration file
selectCommand :: Command -> Bool -> FilePath -> IO ()
selectCommand (Send maybeNum Nothing Nothing input) _ file = fromCLI input (fromMaybe 15 maybeNum) file
selectCommand (Send maybeNum (Just rId) Nothing input) _ file = thread input [] (pure . read $ rId) (fromMaybe 15 maybeNum) file
selectCommand (Send maybeNum Nothing (Just h) input) _ file = thread input h Nothing (fromMaybe 15 maybeNum) file
selectCommand (Send maybeNum (Just rId) (Just h) input) _ file = thread input h (pure . read $ rId) (fromMaybe 15 maybeNum) file
selectCommand (SendInput maybeNum Nothing Nothing) _ file  = fromStdIn (fromMaybe 15 maybeNum) file
selectCommand (SendInput maybeNum (Just rId) (Just h)) _ file = threadStdIn h (pure . read $ rId) (fromMaybe 15 maybeNum) file
selectCommand (SendInput maybeNum (Just rId) Nothing) _ file = threadStdIn [] (pure . read $ rId) (fromMaybe 15 maybeNum) file
selectCommand (SendInput maybeNum Nothing (Just h)) _ file = threadStdIn h Nothing (fromMaybe 15 maybeNum) file
selectCommand (Replies uname twid) c file = putStrLn =<< showReplies uname (fromIntegral twid) c file
selectCommand (MuteReplies uname twid) _ file = do
    muted <- muteRepliers uname (fromIntegral twid) file
    putStrLn "Muted:"
    traverse_ putStrLn muted
selectCommand (MuteMentions uname) _ file =
    getMentions uname file
selectCommand (Timeline maybeNum) c file = putStrLn =<< showTimeline (fromMaybe 11 maybeNum) c file
selectCommand (Mentions maybeNum) c file = putStrLn =<< showTweets c <$> mentions (fromMaybe 11 maybeNum) file
selectCommand (Profile maybeNum n False False) c file = putStrLn =<< showProfile (fromMaybe mempty n) (fromMaybe 11 maybeNum) c file
selectCommand (Profile maybeNum n True False) c file = putStrLn =<< showFilteredTL [filterReplies] (fromMaybe mempty n) (fromMaybe 11 maybeNum) c file
selectCommand (Profile maybeNum n False True) c file = putStrLn =<< showFilteredTL [filterRTs] (fromMaybe mempty n) (fromMaybe 11 maybeNum) c file
selectCommand (Profile maybeNum n True True) c file = putStrLn =<< showFilteredTL [filterReplies, filterRTs] (fromMaybe mempty n) (fromMaybe 11 maybeNum) c file
selectCommand (Sort n maybeNum False) c file = putStrLn =<< showBest' n (fromMaybe 11 maybeNum) c file
selectCommand (Sort n maybeNum True) c file = putStrLn =<< showBest n (fromMaybe 11 maybeNum) c file
selectCommand (List maybeNum n) c file = putStrLn =<< showFavorites (fromMaybe 11 maybeNum) n c file
selectCommand (Markov n) _ file = do
    raw <- getMarkov n Nothing file
    appendFile (n ++ ".txt") (unlines raw)
    putStrLn $ "Written output to: " ++ n ++ ".txt"
selectCommand (Delete n) c file = do
    putStrLn "Deleted:\n"
    putStrLn =<< showTweets c <$> deleteTweetResponse n file
selectCommand (Fav n) c file = do
    putStrLn "Favorited:\n"
    putStrLn =<< showTweets c <$> favoriteTweetResponse n file
selectCommand (Unfav n) c file = do
    putStrLn "Unfavorited:\n"
    putStrLn =<< showTweets c <$> unfavoriteTweetResponse n file
selectCommand (Retweet n) c file = do
    putStrLn "Retweeted:\n"
    putStrLn =<< showTweets c <$> retweetTweetResponse n file
selectCommand (Unretweet n) c file = do
    putStrLn "Unretweeted:\n"
    putStrLn =<< showTweets c <$> unretweetTweetResponse n file
selectCommand (Follow sn) _ file = do
    follow sn file
    putStrLn ("..." ++ sn ++ " followed successfully!")
selectCommand (Unfollow sn) _ file = do
    unfollow sn file
    putStrLn ("..." ++ sn ++ " unfollowed successfully!")
selectCommand (Block sn) _ file = do
    block sn file
    putStrLn ("..." ++ sn ++ " blocked successfully")
selectCommand (Unblock sn) _ file = do
    unblock sn file
    putStrLn ("..." ++ sn ++ " unblocked successfully")
selectCommand (Mute sn) _ file = do
    mute sn file
    putStrLn ("..." ++ sn ++ " muted successfully")
selectCommand (Unmute sn) _ file = do
    unmute sn file
    putStrLn ("..." ++ sn ++ " unmuted successfully")
selectCommand (Dump sn) _ file = BSL.putStrLn =<< getProfileRaw sn 3200 file Nothing

-- | Parser to return a program datatype
program :: Parser Program
program = Program
    <$> hsubparser
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
        <> command "replies" (info repliesParser (progDesc "Show replies to a tweet"))
        <> command "list" (info list (progDesc "List a user's favorites"))
        <> command "dump" (info dump (progDesc "Dump tweets (for debugging)"))
        <> command "block" (info blockParser (progDesc "Block a user"))
        <> command "unblock" (info unblockParser (progDesc "Unblock a user"))
        <> command "mute" (info muteParser (progDesc "Mute a user"))
        <> command "unmute" (info unmuteParser (progDesc "Unmute a user"))
        <> command "mute-replies" (info muteRepliesParser (progDesc "Mute everyone who replied to a particular tweet"))
        <> command "mute-mentions" (info muteMentionsParser (progDesc "Mute everyone who mentioned a particular user"))
        <> command "mentions" (info mentionsParser (progDesc "Fetch mentions")))
    <*> optional (strOption
        (long "cred"
        <> short 'c'
        <> metavar "CREDENTIALS"
        <> completer (bashCompleter "file -X '!*.toml' -o plusdirs")
        <> help "path to credentials"))
    <*> switch
        (long "color"
        <> short 'l'
        <> help "Turn off colorized terminal output.")

-- | Parser for the view subcommand
timeline :: Parser Command
timeline = Timeline
    <$> optional (read <$> strOption
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

-- | Parser for the list subcommand
list :: Parser Command
list = List
    <$> optional (read <$> strOption
        (long "count"
        <> short 'n'
        <> metavar "NUM"
        <> help "Number of tweetInputs to fetch, default 12"))
    <*> user

-- | Parser for the unfollow subcommand
muteParser :: Parser Command
muteParser = Mute <$> user

-- | Parser for the unfollow subcommand
unmuteParser :: Parser Command
unmuteParser = Unmute <$> user

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
getInt = read <$> argument str
    (metavar "TWEET_ID"
    <> help "ID of tweet")

repliesParser :: Parser Command
repliesParser = Replies <$> user <*> getInt

muteRepliesParser :: Parser Command
muteRepliesParser = MuteReplies <$> user <*> getInt

muteMentionsParser :: Parser Command
muteMentionsParser = MuteMentions <$> user

-- | Parser for the user subcommand
profile :: Parser Command
profile = Profile
    <$> optional (read <$> strOption
        (long "count"
        <> short 'n'
        <> metavar "NUM"
        <> help "Number of tweetInputs to fetch, default 12"))
    <*> optional user
    <*> switch (
           long "no-replies"
        <> short 'r'
        <> help "Don't display replies.")
    <*> switch (
           long "no-retweets"
        <> short 't'
        <> help "Don't display retweets.")

-- | Parser for the mention subcommand
mentionsParser :: Parser Command
mentionsParser = Mentions
    <$> optional (read <$> strOption
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
    <*> optional (read <$> strOption
        (long "count"
        <> short 'n'
        <> metavar "NUM"
        <> help "Number of tweetInputs to fetch, default 12"))
    <*> switch
        (long "replies"
        <> short 'r'
        <> help "Include replies in your all-time hits")

-- | Parser for the send subcommand
tweet :: Parser Command
tweet = Send
    <$> optional (read <$> strOption
        (long "tweets"
        <> short 't'
        <> metavar "NUM"
        <> help "Number of tweetInputs in a row, default 15"))
    <*> optional (strOption
        (long "reply"
        <> short 'r'
        <> help "id of status to reply to - be sure to include their handle, e.g. @my_build_errors"))
    <*> optional (some $ strOption
        (long "handle"
        <> short 'h'
        <> metavar "HANDLE1"
        <> help "h to include in replies"))
    <*> (unwords <$> some (argument str
        (metavar "TEXT"
        <> help "text of tweet to be sent")))

-- | Parser for the input command
tweetInput :: Parser Command
tweetInput = SendInput
    <$> optional (read <$> strOption
        (long "tweets"
        <> short 't'
        <> metavar "NUM"
        <> help "Number of tweets in a row, default 15"))
    <*> optional (strOption
        (long "reply"
        <> short 'r'
        <> help "id of status to reply to - be sure to include their handle, e.g. @my_build_errors"))
    <*> optional (some $ argument str
        (metavar "HANDLE1"
        <> help "h to include in replies"))
