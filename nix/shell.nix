{ pkgs }:
pkgs.mkShell {
  buildInputs = [ pkgs.haskellPackages.site pkgs.git ];
}
