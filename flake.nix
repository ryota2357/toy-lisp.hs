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
        lib = pkgs.lib;
        stdenv = pkgs.stdenv;
      in
      {
        packages.default = (hPkgs.callCabal2nix "toy-lisp" ./. { }).overrideAttrs (old: {
          nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.installShellFiles ];
          postInstall = ''
            if ${lib.boolToString (stdenv.buildPlatform.canExecute stdenv.hostPlatform)}; then
              TOY_LISP="$out/bin/toy-lisp"
              PATCH_FISH_COMP_SCRIPT="complete -c toy-lisp -n '__fish_prev_arg_in --script' -a '(__fish_complete_path)'"
              PATCH_ZSH_COMP_SCRIPT="_files" # How to patch zsh completion script only after --script option?
              installShellCompletion --cmd toy-lisp \
                --bash <("$TOY_LISP" --bash-completion-script "$TOY_LISP") \
                --fish <("$TOY_LISP" --fish-completion-script "$TOY_LISP"; echo "$PATCH_FISH_COMP_SCRIPT") \
                --zsh  <("$TOY_LISP" --zsh-completion-script  "$TOY_LISP"; echo "$PATCH_ZSH_COMP_SCRIPT")
            else
              echo "Skipping shell completion installation due to cross-compilation."
            fi
          '';
        });

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
