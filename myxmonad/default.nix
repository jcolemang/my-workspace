
with import <nixpkgs> {};

# { haskellPackages ? (import < nixpkgs> {}).haskellPackages }:

haskell.lib.buildStackProject {
  name = "myxmonad";

  src = ./.;

  buildInputs = [
    cabal-install
    haskellPackages.ghc
    haskellPackages.stack
    zlib
    x11
    xorg.libXext
    xorg.libX11
    xorg.libXrandr
  ];

  meta = {
    description = "My XMonad build";
  };
}
