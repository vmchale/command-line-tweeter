let
  pkgs = import <nixpkgs> { };

  linkWithGold = drv : pkgs.haskell.lib.appendConfigureFlag drv
    "--ghc-option=-optl-fuse-ld=gold --ld-option=-fuse-ld=gold --with-ld=ld.gold";
in
  pkgs.haskell.lib.justStaticExecutables ((pkgs.haskellPackages.callPackage ./default.nix { }))
