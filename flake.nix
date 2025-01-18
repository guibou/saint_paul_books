{
  outputs = { nixpkgs, ... }:
    {
      devShells.x86_64-linux.default = let pkgs = nixpkgs.legacyPackages.x86_64-linux; in pkgs.mkShell {
        buildInputs = [
          (pkgs.haskellPackages.ghcWithPackages (p: [
            p.aeson
            p.PyF
            p.req
            p.lens-regex-pcre
            p.async
          ]))
        ];
      };
    };
}
