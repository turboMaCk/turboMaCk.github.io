{
  description = "My personal website.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    systems.url = "github:nix-systems/x86_64-linux";
    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.systems.follows = "systems";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs {
            inherit system;
            overlays = [ (import ./nix/overlay.nix) ];
          };
      in {
        packages.default = pkgs.haskellPackages.site;
        devShells.default = import ./nix/shell.nix { inherit pkgs; };
      }
    );
}
