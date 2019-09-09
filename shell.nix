let
  pkgs =
    # nixpkgs 19.03 as of 19/08
    import ((import <nixpkgs> {}).fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs-channels";
        rev = "67135fbcc5d5d28390c127ef519b09a362ef2466";
        sha256 = "00591607zmn1hfjs4959ksh164b0gjqwkvbrc4anx6da8xmhfcc2";
    }) {};
  site = import ./default.nix { inherit pkgs; };
in
  pkgs.mkShell {
    buildInputs = [ site ];
  }
