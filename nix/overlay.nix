final: prev: {
  haskellPackages = prev.haskellPackages.override {
    overrides = self: super: {
      site =
        self.callPackage ./site.nix {};
    };
  };

}
