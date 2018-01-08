
with import <nixpkgs> {};

haskell.lib.buildStackProject {
  name = "myxmonad-env";
  buildInputs = [
    cabal-install
    haskellPackages.ghc
    haskellPackages.stack
    zlib
    x11
    xorg.libX11
    xorg.libXext
    xorg.libXrandr
  ];

  libraryPkgconfigDepends = [
  ];
}
