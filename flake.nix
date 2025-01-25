{
  nixConfig = {
    extra-substituters = [ "https://nixcache.reflex-frp.org" ];
    extra-trusted-public-keys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
  };
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    reflex-platform = {
      url = "github:reflex-frp/reflex-platform";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      reflex-platform,
      ...
    }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;

      reflex = import reflex-platform {
        config.android_sdk.accept_license = true;
        config.allowBroken = true;

        haskellOverlays = [
          (
            selfPkgs: superPkgs:
            let
              pkgs = superPkgs.callPackage ({ pkgs }: pkgs) { };
            in
            {
              # Takes hours to check
              RSA = pkgs.haskell.lib.dontCheck superPkgs.RSA;
              # Force static build so android does not fails at link time
              zlib = pkgs.haskell.lib.enableCabalFlag superPkgs.zlib "bundled-c-zlib";
            }
          )
        ];
      };
    in
    {
      apps.x86_64-linux = {
        default = {
          type = "app";
          program = "${self.packages.x86_64-linux.default}/bin/summerize";
        };
        ui = {
          type = "app";
          program = "${self.packages.x86_64-linux.default}/bin/st-paul-books";
        };
      };

      packages.x86_64-linux = {
        default = pkgs.haskellPackages.callCabal2nix "st-paul-book" ./. { };

        ui = reflex.project (
          { pkgs, ... }:
          {
            useWarp = false;
            packages = {
              st-paul-books = ./.;
            };

            shells = {
              ghc = [ "st-paul-books" ];
            };

            android.st-paul-books = {
              executableName = "st-paul-books";
              applicationId = "org.guibou.StPaulBooks";
              displayName = "Saint Paul Books";

              permissions = ''
                <uses-permission android:name="android.permission.INTERNET" />
                <uses-permission android:name="android.permission.ACCESS_NETWORK_STATE" />
              '';
            };
          }
        );
      };
      devShells.x86_64-linux = {
        default = self.packages.x86_64-linux.default.env.overrideAttrs (old: {
          nativeBuildInputs = old.nativeBuildInputs ++ [
            pkgs.cabal-install
            pkgs.haskellPackages.haskell-language-server
          ];
        });
      };
    };
}
