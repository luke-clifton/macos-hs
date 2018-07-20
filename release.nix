{nixpkgs ? import <nixpkgs> {}}:
let
  compilers = ["default" "ghc822" "ghc843"];
in
  with nixpkgs.lib;
  genAttrs compilers (compiler: import ./. {inherit nixpkgs compiler;})
