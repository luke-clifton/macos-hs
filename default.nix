{nixpkgs ? import <nixpkgs> {}, compiler ? "default"}:
let
  haskellPackages =
    if compiler == "default"
    then nixpkgs.haskellPackages
    else nixpkgs.haskell.packages.${compiler};
in
  haskellPackages.callPackage ./macos.nix {inherit (nixpkgs.darwin) Security;}
