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
      packages.x86_64-linux = rec {
        # I need to test wasm, in GHC 10.0 it seems that we can directly
        # iterate in GHCi with it

        # Meaning, no more jsaddle-warp, and direct test in the browser instead
        # of using chromium.
        javascript_pkg = pkgs.pkgsCross.ghcjs.haskellPackages.callCabal2nix "st-paul-book" ./. { };
        javascript = pkgs.runCommand "js-release" {}
        ''
          mkdir $out
          cp ${javascript_pkg}/bin/st-paul-books.jsexe/all.js $out
          cp ${javascript_pkg}/bin/st-paul-books.jsexe/index.html $out
          cp ${./static}/* $out
        '';
        default = pkgs.haskellPackages.callCabal2nix "st-paul-book" ./. { };
      };

      devShells.x86_64-linux = {
        # Basic dev-shell with cabal/hls and the dependencies for this package,
        # everything built by cabal, that's easier.
        default = packages.x86_64-linux.default.env.overrideAttrs (old: {
          nativeBuildInputs = old.nativeBuildInputs ++ (
            with pkgs; [
              cabal-install
              haskellPackages.haskell-language-server
            ]
          );
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
