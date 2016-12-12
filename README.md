#CLIT - Command Line Interface Tweeter

##Config
Generate a token to authorize access to your twitter account by following the guide [here](https://dev.twitter.com/oauth/overview/application-owner-access-tokens)

Then place your API keys, and tokens in `src/Tweet.hs` (on lines 44-51)

##Installation
Install [haskell stack](https://docs.haskellstack.org/en/stable/README/#how-to-install); on unix systems this is as simple as
```wget -qO- https://get.haskellstack.org/ | sh```

Then type `stack install` in the directory and it will generate an executable called `shitpost`, which is what we want.

##Use
To tweet from stderr, run a command like that pipes stderr to stdin
```YOUR_BUILD_COMMAND 2>&1 >/dev/null | shitpost```
