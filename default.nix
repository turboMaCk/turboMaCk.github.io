let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: {
          site =
            self.callPackage ./site.nix {};
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
