let
  config = {
    packageOverrides = pkgs: rec {
      libsass = pkgs.libsass.overrideAttrs (old: rec {
        version = "3.6.3";
        src = pkgs.fetchFromGitHub {
            owner = "sass";
            repo = old.pname;
            rev = version;
            url = "https://github.com/sass/libsass/archive/${version}.tar.gz";
            sha256 = "1q6lvd8sj5k5an32qir918pa5khhcb8h08dzrg1bcxmw7a23j514";
            # Remove unicode file names which leads to different checksums on HFS+
            # vs. other filesystems because of unicode normalisation.
            extraPostFetch = ''
                rm -r $out/test/e2e/unicode-pwd
            '';
        };
      });

      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: {
          hlibsass = super.hlibsass.override {
            inherit libsass;
          };
          hakyll = super.hakyll.overrideAttrs(old: {
            configureFlags = "-f watchServer -f previewServer";
            patches = [./hakyll.patch];
          });
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
