let
  pkgs = import <nixpkgs> { };
  site = import ./default.nix { inherit pkgs; };
in
  pkgs.mkShell {
    buildInputs = [ site ];
  }
