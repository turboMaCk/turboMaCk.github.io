{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callPackage ./site.nix {}
