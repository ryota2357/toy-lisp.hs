{
  description = "Learning Haskell by implementing a Lisp interpreter.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      treefmt-nix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hPkgs = pkgs.haskell.packages."ghc984"; # Stackage LTS 23.5
      in
      {
        packages.default = hPkgs.callCabal2nix "toy-lisp" ./. {
          text = hPkgs.text_2_1_2;
          # containers = hPkgs.containers_0_7;
        };

        devShells.default = pkgs.mkShellNoCC {
          packages = [
            hPkgs.stylish-haskell
            hPkgs.hlint
            hPkgs.haskell-language-server
            pkgs.nil
            pkgs.sbcl
          ];

          buildInputs = [
            hPkgs.cabal-install
            hPkgs.ghc
            pkgs.zlib
          ];
        };

        formatter = treefmt-nix.lib.mkWrapper pkgs {
          projectRootFile = "flake.nix";
          programs = {
            cabal-fmt.enable = true;
            nixfmt.enable = true;
            stylish-haskell.enable = true;
            prettier = {
              enable = true;
              includes = [ "*.md" ];
            };
          };
          settings.global.excludes = [
            ".envrc"
            ".github/workflows/ci.yaml"
            "LICENSE"
          ];
        };
      }
    );
}
