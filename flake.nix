{
  description = "Advent Calender 2023";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    let
      system = flake-utils.lib.system.x86_64-linux;
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
    in {
      packages.${system} = { inherit day01 day02 day03; };
      apps.${system} = {
        day01p1 = {
          type = "app";
          program = "${day01}/bin/part1";
        };
        day01p2 = {
          type = "app";
          program = "${day01}/bin/part2";
        };
        day02p1 = {
          type = "app";
          program = "${day02}/bin/part1";
        };
        day02p2 = {
          type = "app";
          program = "${day02}/bin/part2";
        };
        day03p1 = {
          type = "app";
          program = "${day03}/bin/part1";
        };
        day03p2 = {
          type = "app";
          program = "${day03}/bin/part2";
        };
      };
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = myDevTools;
        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
      };
    };
}
