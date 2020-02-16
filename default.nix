let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          hakyll = haskellPackagesOld.hakyll.overrideAttrs( old: {
            configureFlags = "-f watchServer -f previewServer";
            patches = [./hakyll.patch];
          });
          site =
            haskellPackagesNew.callPackage ./site.nix {};
        };
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };
in {
  site = pkgs.haskellPackages.site;
  env = pkgs.haskellPackages.site.env;
  pkgs = pkgs;
}
