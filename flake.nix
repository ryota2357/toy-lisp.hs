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

        # Wrap Stack to work with our Nix integration. We don't want to modify stack.yaml so non-Nix users don't notice anything.
        # --no-nix          # We don't want Stack's way of integrating Nix.
        # --system-ghc      # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in
      {
        devShells.default = pkgs.mkShellNoCC rec {
          packages = [
            hPkgs.ghcid # Continuous terminal Haskell compile checker
            hPkgs.ormolu # Haskell formatter
            # hPkgs.hlint # Haskell codestyle checker
            # hPkgs.hoogle # Lookup Haskell documentation
            hPkgs.haskell-language-server # LSP server for editor
            # hPkgs.implicit-hie # auto generate LSP hie.yaml file from cabal
            # hPkgs.retrie # Haskell refactoring tool
            # hPkgs.cabal-install
            stack-wrapped
            pkgs.nil
          ];

          buildInputs = [
            hPkgs.ghc
            pkgs.zlib
          ];

          # Make external Nix c libraries like zlib known to GHC, like pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/f348e86735cf6a1822a7fd203b500787d8efa9b0/pkgs/development/haskell-modules/generic-stack-builder.nix#L55
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
        };

        formatter = treefmt-nix.lib.mkWrapper pkgs {
          projectRootFile = "flake.nix";
          programs = {
            nixfmt.enable = true;
            ormolu.enable = true;
            yamlfmt.enable = true;
          };
          settings.global.excludes = [
            ".envrc"
          ];
        };
      }
    );
}
