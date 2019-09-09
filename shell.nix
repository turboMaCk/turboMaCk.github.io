let
  pkgs =
    # nixpkgs 19.03 as of 09/09/2019
    import ((import <nixpkgs> {}).fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs-channels";
        rev = "5b0b58685d68b9c9c12132f7b392c9dcdecc5931";
        sha256 = "1fy1ghl79p3w1vg0hnakvrb1lqic4mjrwmylcgqa5bd4ilfrirzf";
    }) {};
  site = import ./default.nix { inherit pkgs; };
in
  pkgs.mkShell {
    buildInputs = [ site ];
  }
