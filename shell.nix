let
  vals = import ./default.nix;
in
vals.pkgs.mkShell {
  buildInputs = [ vals.site ];
}
