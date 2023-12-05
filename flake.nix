{
  description = "Advent Calender 2023";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        compiler = "ghc927";
        pkgs = import nixpkgs { inherit system; };
        hPkgs = pkgs.haskell.packages."${compiler}";
        myDevTools = with hPkgs; [
          ghc
          ghcid
          fourmolu
          hlint
          hoogle
          haskell-language-server
          implicit-hie
          retrie
          cabal-install
          cabal2nix
          pkgs.zlib
          hspec
        ];
        day01 = with pkgs; haskell.packages.${compiler}.callPackage ./01 { };
        day02 = with pkgs; haskell.packages.${compiler}.callPackage ./02 { };
        day03 = with pkgs; haskell.packages.${compiler}.callPackage ./03 { };
        day04 = with pkgs; haskell.packages.${compiler}.callPackage ./04 { };
        day05 = with pkgs; haskell.packages.${compiler}.callPackage ./05 { };
        mkApp = day: part:
          {
            program = "${day}/bin/part${part}";
          } // {
            type = "app";
          };
      in {
        packages = { inherit day01 day02 day03; };
        apps = {
          day01p1 = mkApp "${day01}" "1";
          day01p2 = mkApp "${day01}" "2";
          day02p1 = mkApp "${day02}" "1";
          day02p2 = mkApp "${day02}" "2";
          day03p1 = mkApp "${day03}" "1";
          day03p2 = mkApp "${day03}" "2";
          day04p1 = mkApp "${day04}" "1";
          day04p2 = mkApp "${day04}" "2";
          day05p1 = mkApp "${day05}" "1";
          day05p2 = mkApp "${day05}" "2";
        };
        devShells.default = pkgs.mkShell {
          buildInputs = myDevTools;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
        };
      });
}
