{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    reflex = {
      url = "github:reflex-frp/reflex-platform";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, ... }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux; in
    {
      packages.x86_64-linux = {
        default = pkgs.haskellPackages.callCabal2nix "st-paul-book" ./. { };
      };
      devShells.x86_64-linux.default = self.packages.x86_64-linux.default.env.overrideAttrs (old: {
        nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.cabal-install pkgs.haskellPackages.haskell-language-server ];

      });
    };
}
