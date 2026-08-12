{
  description = "Advent of Code - Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Haskell toolchain
            haskell.compiler.ghc912
            haskell.packages.ghc912.haskell-language-server
            cabal-install

            # System libraries needed by dependencies (e.g. hmatrix)
            lapack
            blas
            zlib
            pkg-config
          ];

          shellHook = ''
            export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath (with pkgs; [ lapack blas zlib ])}:$LD_LIBRARY_PATH"
          '';
        };
      });
}
