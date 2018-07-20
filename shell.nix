{ nixpkgs ? import <nixpkgs> {}, compiler ? "default"}:
(import ./. {inherit nixpkgs compiler;}).env
