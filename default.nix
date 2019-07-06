{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callPackage ./blog.nix {}
