{
  description = "Scarf Gateway Nix Flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/171068ed83c5d45e376e5155525f8c07fe2354b4";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [self.overlay];
      });

    in {
      overlay = final: prev: {};
      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
        in
        {
          # Etag changes during test, and giving false negative result. So not running test nix.
          gateway = pkgs.haskell.lib.dontCheck (pkgs.haskellPackages.callCabal2nix "gateway" ./. {});
          generate-changelog = pkgs.github-changelog-generator;
          default = self.packages.${system}.gateway;
        }
      );

      devShells = forAllSystems(system:
        let
          pkgs = nixpkgsFor.${system};
          libs = with pkgs; [
            pcre
            zlib
            openssl
          ];

        in {
          default = pkgs.mkShell {
            packages = [];
            buildInputs = with pkgs; [
              cabal-install
              haskell.compiler.ghc94
              ghcid
            ] ++ libs;
          shellHook = "export PS1='[$PWD]\n❄ '";
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;
          };
        });
    };
}
