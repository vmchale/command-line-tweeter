# Command Line Interface Tweeter

[![Build Status](https://travis-ci.org/vmchale/command-line-tweeter.svg?branch=master)](https://travis-ci.org/vmchale/command-line-tweeter)

![Displaying a user timeline in a terminal.](https://raw.githubusercontent.com/vmchale/command-line-tweeter/master/screenshot.png)

`tweet-hs` is a command-line tool for twitter. It has more features than
its [rust counterpart](https://github.com/vmchale/clit-rs) and it's a bit
slower. 

Reasons to use tweeth-s:
  - Faster than other tools ([t](https://github.com/sferik/t),
  [oysttyer](https://github.com/oysttyer/oysttyer))
  - Support for colored output. 
  - Can be used in scripts
  - You know haskell and like being able to extend your tools. 
  - You want something that can be called from
    [vim](https://github.com/vmchale/vim-twitter)
  - You want a twitter library for haskell. 
  - BSD3 licensed 

Reasons not to use tweet-hs:
  - You want "twitter in a terminal" that [rainbowtools](https://github.com/DTVD/rainbowstream)
    or [oysttyer](https://github.com/oysttyer/oysttyer) provides. 
  - You want to be able to easily tweet emoji

## Comparison to other command-line clients

| Tool | Language | Color output | Interactive | Vim plugin support | Scriptable | Send emoji |
| ---- | -------- | ------------ | ----------- | ------------------ | ---------- | ---------- |
| tw | Rust | x |   | x | x |  |
| rainbowstream | Python | x | x |  |  | x |
| oysttyer | Perl |  | x |  | ½ |  |
| tweet-hs | Haskell | x |  | x | x |  |
| t | Ruby | ½ |  |  | x |  |

## Config
Generate a token to authorize access to your twitter account by following the guide [here](https://dev.twitter.com/oauth/overview/application-owner-access-tokens)

Then place your API keys and OAuth tokens in a file `~/.cred.toml`, as in the
following example:

```
api-key = "API_KEY_HERE"
api-sec = "API_SECRET_HERE"
tok = "OAUTH_TOKEN_HERE"
tok-sec = "TOKEN_SECRET_HERE"
```

## Installation

If you're on Linux/Windows the best way is probably to download the binaries
from the releases page [here](https://github.com/vmchale/command-line-tweeter/releases).

To build from source, install [haskell stack](https://docs.haskellstack.org/en/stable/README/#how-to-install); on unix systems this is as simple as

```
wget -qO- https://get.haskellstack.org/ | sh
```

Then type `stack install tweet-hs` it will put an executable called `tweet` on your path.

## Use

### View Profiles and timelines

To get your timeline, simply type:

```
tweet view
```

To view a user's profile, type e.g.

```
tweet user NateSilver538 --color
```

### Sending tweets

To send a tweet:

```
tweet send "This is my tweet"
```

#### Input from stdin
To tweet from stderr, run a command that pipes stderr to stdin, i.e.

```
stack build &>/dev/null | tweet input
```

The `tweet` executable reads from stdin only, but you can view the options (replies, number of tweets to thread, etc.) with

```
tweet --help
```

This script powers the twitter account [@my\_build\_errors](https://twitter.com/my_build_errors) for instance. There's an example bash script for in `bash/example`

### Viewing your timeline

You can also use

```
tweet view
```

or 

```
tweet view --color
```

to view your own timeline.

### GHCi integration

You can define the following in your `~/.ghci`

```haskell
:def tweet (\str -> pure $ ":! tweet send \"" ++ str ++ "\"")
```

### Completions

The directory `bash/` has a `mkCompletions` script to allow command completions for your convenience.

## Library

A haskell package is included. It's fairly easy to use once you have the credentials set up, with two main functions: `thread` and `basicTweet`: the first for threading your own tweets or replying to someone else's and the second for just tweeting.
