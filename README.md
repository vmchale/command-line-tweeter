# Command Line Interface Tweeter

## Config
Generate a token to authorize access to your twitter account by following the guide [here](https://dev.twitter.com/oauth/overview/application-owner-access-tokens)

Then place your API keys and OAuth tokens in a file `.cred`, separated by a line break:

```
api-key: API_KEY_HERE
api-sec: API_SECRET_HERE
tok: OAUTH_TOKEN_HERE
tok-sec: TOKEN_SECRET_HERE
```

## Installation

Install [haskell stack](https://docs.haskellstack.org/en/stable/README/#how-to-install); on unix systems this is as simple as

```
wget -qO- https://get.haskellstack.org/ | sh
```

Then type `stack install` in the directory and it will generate an executable called `tweet`, which is what we want.

## Use
To tweet from stderr, run a command that pipes stderr to stdin, i.e.

```
YOUR_BUILD_COMMAND 2>&1 >/dev/null | tweet
```

This script powers the twitter account [@my\_build\_errors](https://twitter.com/my_build_errors) for instance. There's an example bash script for in `bash/example`

## Library
A haskell package is included. It's fairly easy to use once you have the credentials set up, with two main functions: `thread` and `basicTweet`: the first for threading your own tweets or replying to someone else's and the second for just tweeting.

### Finer details
The function `tweetData` will tweet an object of type `Tweet`. Its use is pretty self-explanatory, but how to best form `Tweet`s is non immediately obvious.

`Tweet` is an instance of `Default` so you can use `def` to get an empty tweet replying to nobody and not fetching extended user data. This is especially useful if you want to use lenses and avoid ugly record syntax, e.g.

```
over (status) (const "This is the new status field) $ def
```

will give you a `Tweet` with sensible defaults and the desired text.
