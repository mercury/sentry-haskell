{
  description = "";
  outputs =
    inputs@{ self, nixpkgs }:
    let
      inherit (import ./nix/dev/util.nix { inherit inputs; })
        forEachSystem
        loadPrivateFlake
        ;
      privateInputs = (loadPrivateFlake ./nix/dev/private).inputs;
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
        "aarch64-linux"
      ];
      overlays = [ self.overlays.native ];
      eachSystem = forEachSystem { inherit systems overlays; };
    in
    {
      # public outputs, should not reference
      overlays = {
        native = import ./nix/overlays/native.nix;
      };

      # (private) dev outputs, should not be imported by downstream consumers
      # as they may depend on private flake inputs.
      devShells = eachSystem (
        { pkgs, ... }:
        {
          default = pkgs.mkShell {
            buildInputs =
              (with pkgs; [
                # tooling
                cabal-install
                ghciwatch
                haskell.compiler.ghc910
                hpack
                just
                specify-cli
                # integration test server
                kent-server
                # C library dependencies
                zlib.dev
              ])
              ++ (with pkgs.haskell.packages.ghc910; [
                profiterole
                profiteur
              ]);
          };
        }
      );
      apps = eachSystem (
        { pkgs, ... }:
        {
          update-dev-private-narHash = {
            type = "app";
            program = "${pkgs.writeShellScript "update-dev-private-narHash" ''
              nix --extra-experimental-features "nix-command flakes" flake lock ./dev/private
              nix --extra-experimental-features "nix-command flakes" hash path ./dev/private | tr -d '\n' > ./dev/private.narHash
            ''}";
          };
        }
      );
      formatter = eachSystem (
        { pkgs, ... }:
        let
          eval = cfg: (privateInputs.treefmt-nix.lib.evalModule pkgs cfg).config.build.wrapper;
        in
        eval {
          projectRootFile = "flake.nix";
          programs = {
            fourmolu.enable = true;
            nixfmt.enable = true;
          };
          # Globally exclude the following patterns from auto-formatting:
          settings.global.excludes = [
            # ...jujutsu's VCS tracking directory
            ".jj/*"
            # ...all markdown files
            "*.md"
          ];
        }
      );
    };
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
}
