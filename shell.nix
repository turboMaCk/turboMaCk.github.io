let
  config = {
    # hakyll-sass is marked as broken but works
    # https://github.com/NixOS/nixpkgs/pull/77107
    allowBroken = true;
  };
  pkgs =
    # nixpkgs unstable as of 06/01/2020
    import ((import <nixpkgs> {}).fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs-channels";
        rev = "2e8fc97dbfacc1178ae846ab31210539f7a958f0";
        sha256 = "1api5vypvw5pihz3kjps8q5hxf02ncgxg6axnpr2lcbc6m14s40q";
    }) { inherit config; };
  site = import ./default.nix { inherit pkgs; };
in
  pkgs.mkShell {
    buildInputs = [ site ];
  }
