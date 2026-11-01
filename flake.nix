{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs =
    { self
    , nixpkgs
    , ...
    }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in
    rec {
      packages.x86_64-linux = {
        javascript = pkgs.pkgsCross.ghcjs.haskellPackages.callCabal2nix "st-paul-book" ./. { };
        default = pkgs.haskellPackages.callCabal2nix "st-paul-book" ./. { };
      };

      devShells.x86_64-linux = {
        # Basic dev-shell with cabal/hls and the dependencies for this package,
        # everything built by cabal, that's easier.
        default = packages.x86_64-linux.default.env.overrideAttrs (old: {
          nativeBuildInputs = old.nativeBuildInputs ++ (with pkgs; [ cabal-install haskell-language-server ]);
        });

        js = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.cabal-install
            pkgs.pkgsCross.ghcjs.buildPackages.ghc
            pkgs.nodejs
          ];
        };
      };
    };
}
