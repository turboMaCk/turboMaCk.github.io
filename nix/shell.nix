{ pkgs }:
pkgs.mkShell {
  buildInputs = [ pkgs.haskellPackages.site ];
}
