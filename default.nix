let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          hakyll = haskellPackagesOld.hakyll.overrideAttrs(old: {
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
