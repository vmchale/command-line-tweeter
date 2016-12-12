#CLIT - Command Line Interface Tweeter

##Config
Generate a token to authorize access to your twitter account by following the guide [here](https://dev.twitter.com/oauth/overview/application-owner-access-tokens)

Then place your API keys, and tokens in the file `.cred` in the correct order, separated by a line break:

```
api-key: API_KEY_HERE
api-sec: API_SECRET_HER
tok:     OAUTH_TOKEN_HERE
tok-sec: TOKEN_SECRET_HER
```

##Installation

Install [haskell stack](https://docs.haskellstack.org/en/stable/README/#how-to-install); on unix systems this is as simple as

```
wget -qO- https://get.haskellstack.org/ | sh
```

Then type `stack install` in the directory and it will generate an executable called `tweet`, which is what we want.

##Use
To tweet from stderr, run a command like that pipes stderr to stdin

```YOUR_BUILD_COMMAND 2>&1 >/dev/null | tweet```

This program powers the twitter account [@my_build_errors](https://twitter.com/my_build_errors)
