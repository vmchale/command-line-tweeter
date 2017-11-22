bench:
    bench "tw user -n13" "tweet user -n13"
# t timeline

check:
    git diff master origin/master

next:
    @export VERSION=$(cat tweet-hs.cabal | grep -P -o '\d+\.\d+\.\d+\.\d+' tweet-hs.cabal | head -n1 | awk -F. '{$NF+=1; print $0}' | sed 's/ /\./g') && echo $VERSION && sed -i "2s/[0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+/$VERSION/" tweet-hs.cabal
    @git commit -am "release/CI"

ci:
    cabal new-build
    cabal new-test
    cabal new-bench

size:
    sn d $(fd 'tweet$' -I | tail -n1)

upload:
    rm -rf dist/
    cabal sdist
    cabal upload --publish $(fd '\.tar\.gz$' -I)

release:
    git tag "$(grep -P -o '\d+\.\d+\.\d+\.\d+' tweet-hs.cabal | head -n1)"
    git push origin --tags
    git tag -d "$(grep -P -o '\d+\.\d+\.\d+\.\d+' tweet-hs.cabal | head -n1)"

install:
    @cabal new-build --constraint='tweet-hs +llvm-fast'
    @cp $(fd 'tweet$' -I | tail -n1) ~/.local/bin

name:
    github-release edit -s $(cat .git-token) -u vmchale -r command-line-tweeter -n "$(madlang run ~/programming/madlang/releases/releases.mad)" -t "$(grep -P -o '\d+\.\d+\.\d+\.\d+' tweet-hs.cabal | head -n1)"

clean:
    sn c .
    rm -f .ghc.environment.x86_64-linux-8.2.1 man/tweet-hs.1 tags
