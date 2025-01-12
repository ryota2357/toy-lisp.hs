{
  description = "my project description";

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
        hPkgs = pkgs.haskell.packages."ghc966"; # need to match Stackage LTS version from stack.yaml snapshot
      in
      {
        devShells.default = pkgs.mkShellNoCC {
          packages = [
            hPkgs.stylish-haskell # Haskell formatter
            hPkgs.hlint # Haskell codestyle checker
            # hPkgs.hoogle # Lookup Haskell documentation
            hPkgs.haskell-language-server # LSP server for editor
            # hPkgs.implicit-hie # auto generate LSP hie.yaml file from cabal
            # hPkgs.retrie # Haskell refactoring tool
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
            "LICENSE"
          ];
        };
      }
    );
}
